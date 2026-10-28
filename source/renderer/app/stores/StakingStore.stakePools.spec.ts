import { observable, runInAction } from 'mobx';
import StakingStore from './StakingStore';
import { STAKE_POOLS_INTERVAL } from '../config/stakingConfig';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { AnalyticsTracker } from '../analytics';

jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

const POOL = { id: 'pool1' };

const buildStore = (listStakePools: jest.Mock) => {
  const listener = { listen: jest.fn() };
  const actions = new Proxy(
    {},
    { get: () => new Proxy({}, { get: () => listener }) }
  ) as unknown as ActionsMap;
  const store = new StakingStore(
    {
      ada: { getStakePools: listStakePools },
      localStorage: {
        getStakingInfoWasOpen: jest.fn().mockResolvedValue(false),
        getStakePoolsListViewTooltip: jest.fn().mockResolvedValue(false),
      },
    } as unknown as Api,
    actions,
    { sendEvent: jest.fn() } as unknown as AnalyticsTracker
  );
  const networkStatus = observable({
    isSynced: false,
    isConnected: true,
    isShelleyActivated: true,
  });
  store.configure({ networkStatus } as unknown as StoresMap);
  store.setup();
  store._reactions.forEach((reaction) => reaction.start());
  const sync = (isSynced: boolean) =>
    runInAction(() => {
      networkStatus.isSynced = isSynced;
    });
  return { store, networkStatus, sync };
};

describe('StakingStore stake pools on demand', () => {
  let stores: Array<StakingStore> = [];
  beforeEach(() => {
    jest.useFakeTimers();
  });
  afterEach(() => {
    stores.forEach((store) => store.teardown());
    stores = [];
    jest.clearAllTimers();
    jest.useRealTimers();
  });

  const create = () => {
    const getStakePools = jest.fn().mockResolvedValue([POOL]);
    const built = buildStore(getStakePools);
    stores.push(built.store);
    return { ...built, getStakePools };
  };
  // Request applies a result in a zero-delay timer.
  const settle = async () => {
    for (let i = 0; i < 5; i += 1) {
      // eslint-disable-next-line no-await-in-loop
      await Promise.resolve();
      jest.advanceTimersByTime(1);
    }
  };

  it('fetches nothing at setup or when the node reaches the tip', async () => {
    const { sync, getStakePools } = create();
    await settle();
    sync(true);
    await settle();
    jest.advanceTimersByTime(10 * STAKE_POOLS_INTERVAL);
    await settle();
    expect(getStakePools).not.toHaveBeenCalled();
  });

  it('fetches once on acquire when the node is synced', async () => {
    const { store, sync, getStakePools } = create();
    sync(true);
    store.acquireStakePools();
    await settle();
    expect(getStakePools).toHaveBeenCalledTimes(1);
    expect(store.stakePools).toEqual([POOL]);
  });

  it('waits for the tip before fetching what a screen asked for', async () => {
    const { store, sync, getStakePools } = create();
    store.acquireStakePools();
    await settle();
    expect(getStakePools).not.toHaveBeenCalled();
    sync(true);
    await settle();
    expect(getStakePools).toHaveBeenCalled();
  });

  it('polls while held and stops polling at release', async () => {
    const { store, sync, getStakePools } = create();
    sync(true);
    const release = store.acquireStakePools();
    await settle();
    jest.advanceTimersByTime(STAKE_POOLS_INTERVAL);
    await settle();
    const whileHeld = getStakePools.mock.calls.length;
    expect(whileHeld).toBeGreaterThan(1);

    release();
    await settle();
    const afterRelease = getStakePools.mock.calls.length;
    jest.advanceTimersByTime(10 * STAKE_POOLS_INTERVAL);
    await settle();
    expect(getStakePools.mock.calls.length).toBe(afterRelease);
    expect(store.pollingStakePoolsInterval).toBeNull();
    expect(store.refreshPolling).toBeNull();
  });

  it('keeps polling until the last of two holders releases', async () => {
    const { store, sync, getStakePools } = create();
    sync(true);
    const first = store.acquireStakePools();
    const second = store.acquireStakePools();
    await settle();
    first();
    await settle();
    const before = getStakePools.mock.calls.length;
    jest.advanceTimersByTime(STAKE_POOLS_INTERVAL);
    await settle();
    expect(getStakePools.mock.calls.length).toBeGreaterThan(before);
    second();
    await settle();
    const after = getStakePools.mock.calls.length;
    jest.advanceTimersByTime(3 * STAKE_POOLS_INTERVAL);
    await settle();
    expect(getStakePools.mock.calls.length).toBe(after);
  });

  it('withdraws an ask once however often its release is called', async () => {
    const { store, sync } = create();
    sync(true);
    const first = store.acquireStakePools();
    store.acquireStakePools();
    first();
    first();
    await settle();
    expect(store._stakePoolDemand).toBe(1);
  });

  it('keeps the list in memory after the last release', async () => {
    const { store, sync } = create();
    sync(true);
    const release = store.acquireStakePools();
    await settle();
    release();
    await settle();
    expect(store.stakePools).toEqual([POOL]);
    expect(store.getStakePoolById('pool1')).toEqual(POOL);
  });

  it('stops fetching when the node leaves the tip', async () => {
    const { store, sync, getStakePools } = create();
    sync(true);
    store.acquireStakePools();
    await settle();
    sync(false);
    await settle();
    const calls = getStakePools.mock.calls.length;
    jest.advanceTimersByTime(3 * STAKE_POOLS_INTERVAL);
    await settle();
    expect(getStakePools.mock.calls.length).toBe(calls);
  });

  it('does not fetch on a change of SMASH server with no screen asking', async () => {
    const { store, sync, getStakePools } = create();
    sync(true);
    store.updateSmashSettingsRequest.execute = jest
      .fn()
      .mockReturnValue({ promise: Promise.resolve() }) as never;
    await store._selectSmashServerUrl({ smashServerUrl: 'https://smash' });
    await settle();
    expect(getStakePools).not.toHaveBeenCalled();
  });
});
