import React from 'react';
import BigNumber from 'bignumber.js';
import { observer, Provider } from 'mobx-react';
import { runInAction } from 'mobx';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import { TestDecorator } from '../../../../../../tests/_utils/TestDecorator';
import DelegationSetupWizardDialogContainer from './DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import UndelegateWalletDialogContainer from '../../wallet/dialogs/settings/UndelegateWalletDialogContainer';
import UiDialogsStore from '../../../stores/UiDialogsStore';
import HardwareWalletsStore from '../../../stores/HardwareWalletsStore';
import TransactionsStore from '../../../stores/TransactionsStore';
import DialogsActions from '../../../actions/dialogs-actions';
import Action from '../../../actions/lib/Action';
import { noopAnalyticsTracker } from '../../../analytics';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import { DeviceTypes } from '../../../../../common/types/hardware-wallets.types';
import {
  getHardwareWalletTransportChannel,
  resetTrezorActionChannel,
} from '../../../ipc/getHardwareWalletChannel';
import { formattedWalletAmount } from '../../../utils/formatters';
import type { Api } from '../../../api';
import type { ActionsMap } from '../../../actions';
import type { StoresMap } from '../../../stores';
import type { CoinSelectionsResponse } from '../../../api/transactions/types';

jest.mock('@trezor/connect', () => ({}));
jest.mock('lodash-es/util', () => ({
  times: jest.requireActual('lodash/times'),
}));
jest.mock('../../../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const deferred = <T,>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((done) => {
    resolve = done;
  });
  return { promise, resolve };
};
const selection: CoinSelectionsResponse = {
  inputs: [],
  outputs: [],
  certificates: [],
  withdrawals: [],
  metadata: null,
  deposits: new BigNumber(2),
  depositsReclaimed: new BigNumber(2),
  fee: new BigNumber('0.17'),
};
const originalPilot = global.cip30Enabled;
let dispose = () => {};

function mountFlow(
  flow: 'quit' | 'join',
  select = jest.fn().mockResolvedValue(selection)
) {
  const wallet = {
    id: 'wallet',
    name: 'Staking Ledger',
    isHardwareWallet: true,
    amount: new BigNumber(20),
    availableAmount: new BigNumber(20),
    reward: new BigNumber(0),
    assets: { total: [] },
    delegatedStakePoolId: 'old-pool',
    pendingDelegations: [],
  };
  const pool = {
    id: 'new-pool',
    ticker: 'NEW',
    name: 'New Pool',
    saturation: 0,
    relativeStake: new BigNumber(0),
    cost: new BigNumber(0),
    pledge: new BigNumber(0),
    potentialRewards: new BigNumber(0),
    nonMyopicMemberRewards: 0,
    profitMargin: 0,
    ranking: 1,
    producedBlocks: 0,
    homepage: '',
    description: '',
    isCharity: false,
  };
  const submit = jest.fn().mockResolvedValue({ id: 'transaction' });
  const api = {
    ada: { selectCoins: select, createExternalTransaction: submit },
    localStorage: {},
  } as unknown as Api;
  const actions = {
    dialogs: new DialogsActions(),
    staking: {
      selectDelegationWallet: new Action(),
      joinStakePool: new Action(),
    },
    wallets: {
      undelegateWallet: new Action(),
      setUndelegateWalletSubmissionSuccess: new Action(),
    },
  } as unknown as ActionsMap;
  const uiDialogs = new UiDialogsStore(api, actions, noopAnalyticsTracker);
  const hardwareWallets = new HardwareWalletsStore(
    api,
    actions,
    noopAnalyticsTracker
  );
  const transactions = new TransactionsStore(
    api,
    actions,
    noopAnalyticsTracker
  );
  const request = { reset: jest.fn(), isExecuting: false, error: null };
  const unifiedJoin = jest.fn().mockResolvedValue(undefined);
  const unifiedQuit = jest.fn().mockResolvedValue(undefined);
  const stores = {
    uiDialogs,
    hardwareWallets,
    transactions,
    wallets: {
      active: wallet,
      allWallets: [wallet],
      getWalletById: (id: string) => (id === wallet.id ? wallet : null),
      withdrawalDecisionFor: () => ({ kind: 'withdraw' }),
      undelegateWalletSubmissionSuccess: false,
      _undelegateWallet: unifiedQuit,
      refreshWalletsData: jest.fn(),
    },
    staking: {
      joinStakePoolRequest: request,
      quitStakePoolRequest: request,
      stakePools: [pool],
      recentStakePools: [],
      maxDelegationFunds: new BigNumber(1000),
      getStakePoolById: () => pool,
      acquireStakePools: () => () => undefined,
      isDelegationTransactionPending: false,
      _joinStakePool: unifiedJoin,
    },
    profile: { currentLocale: 'en-US', currentTheme: 'dark-blue' },
    networkStatus: { futureEpoch: {} },
    app: { openExternalLink: jest.fn() },
  } as unknown as StoresMap;
  uiDialogs.configure(stores);
  uiDialogs.setup();
  hardwareWallets.configure(stores);
  runInAction(() => {
    hardwareWallets.hardwareWalletsLocalDataRequest.result = {
      wallet: {
        id: 'ledger',
        disconnected: false,
        device: { deviceType: DeviceTypes.LEDGER, path: 'ledger' },
      },
    } as unknown as HardwareWalletsStore['hardwareWalletsConnectionData'];
  });
  const Dialog =
    flow === 'quit'
      ? UndelegateWalletDialogContainer
      : DelegationSetupWizardDialog;
  const Host = observer(() => (
    <>
      <button
        onClick={() => {
          actions.dialogs.open.trigger({
            // Legacy actions type class-component identifiers as functions.
            dialog: Dialog as unknown as Parameters<
              DialogsActions['open']['trigger']
            >[0]['dialog'],
          });
          actions.dialogs.updateDataForActiveDialog.trigger({
            data: { walletId: wallet.id, poolId: pool.id },
          });
        }}
      >
        Open staking
      </button>
      {uiDialogs.isOpen(UndelegateWalletDialogContainer) && (
        <UndelegateWalletDialogContainer onExternalLinkClick={() => {}} />
      )}
      {uiDialogs.isOpen(DelegationSetupWizardDialog) && (
        <DelegationSetupWizardDialogContainer />
      )}
    </>
  ));
  const view = render(
    <TestDecorator>
      <BrowserLocalStorageBridge>
        <DiscreetModeFeatureProvider>
          <Provider stores={stores} actions={actions}>
            <Host />
          </Provider>
        </DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </TestDecorator>
  );
  dispose = () => {
    clearInterval(uiDialogs._secondsTimerInterval);
    actions.dialogs.open.removeAll();
    actions.dialogs.closeActiveDialog.removeAll();
    actions.dialogs.resetActiveDialog.removeAll();
    actions.dialogs.updateDataForActiveDialog.removeAll();
  };
  fireEvent.click(screen.getByRole('button', { name: 'Open staking' }));
  return {
    ...view,
    hardwareWallets,
    transactions,
    select,
    submit,
    actions,
    unifiedJoin,
    unifiedQuit,
  };
}

async function reachConfirmation() {
  fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
  fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
  fireEvent.click(await screen.findByRole('button', { name: 'Continue' }));
  await screen.findByRole('button', { name: 'Cancel' });
}

beforeEach(() => {
  global.cip30Enabled = false;
  jest.spyOn(resetTrezorActionChannel, 'request').mockResolvedValue(undefined);
});
afterEach(() => {
  cleanup();
  dispose();
  jest.restoreAllMocks();
  global.cip30Enabled = originalPilot;
});

it('mounts undelegation through real dialog actions, displays wallet/fee, and Cancel releases its hardware queue once', async () => {
  const transport = deferred<null>();
  const device = jest
    .spyOn(getHardwareWalletTransportChannel, 'request')
    .mockReturnValue(transport.promise);
  const fixture = mountFlow('quit');
  expect(await screen.findByLabelText('Modal Dialog')).toHaveTextContent(
    'Staking Ledger'
  );
  await waitFor(() => expect(device).toHaveBeenCalledTimes(1));
  expect(
    screen.getByText(formattedWalletAmount(selection.fee, false))
  ).toBeInTheDocument();
  const reset = jest.spyOn(fixture.hardwareWallets, '_resetTransaction');
  fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
  const next = await fixture.transactions.acquireWalletSendLock('wallet');
  next.release();
  expect(reset).toHaveBeenCalledTimes(1);
  await act(async () => {
    transport.resolve(null);
  });
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(false);
  expect(fixture.submit).not.toHaveBeenCalled();
});

it.each(['quit', 'join'] as const)(
  '%s unmount during selection releases the real queue and never starts a late device request',
  async (flow) => {
    const pending = deferred<CoinSelectionsResponse>();
    const select = jest.fn().mockReturnValue(pending.promise);
    const device = jest
      .spyOn(getHardwareWalletTransportChannel, 'request')
      .mockResolvedValue(null);
    const fixture = mountFlow(flow, select);
    if (flow === 'join') await reachConfirmation();
    await waitFor(() => expect(select).toHaveBeenCalledTimes(1));
    fixture.unmount();
    const next = await fixture.transactions.acquireWalletSendLock('wallet');
    next.release();
    await act(async () => {
      pending.resolve(selection);
      await fixture.hardwareWallets.selectCoinsRequest.promise;
    });
    expect(device).not.toHaveBeenCalled();
    expect(fixture.hardwareWallets.txSignRequest).not.toHaveProperty(
      'coinSelection'
    );
    expect(fixture.submit).not.toHaveBeenCalled();
  }
);

it('wizard reaches real confirmation, and Back cancels owned work without a second reset on unmount', async () => {
  const transport = deferred<null>();
  const device = jest
    .spyOn(getHardwareWalletTransportChannel, 'request')
    .mockReturnValue(transport.promise);
  const fixture = mountFlow('join');
  await reachConfirmation();
  await waitFor(() => expect(device).toHaveBeenCalledTimes(1));
  expect(
    screen.getByText(formattedWalletAmount(selection.fee, false))
  ).toBeInTheDocument();
  const reset = jest.spyOn(fixture.hardwareWallets, '_resetTransaction');
  const back = screen
    .getAllByRole('button', { name: '' })
    .find((button) => button.tabIndex !== -1);
  expect(back).toBeDefined();
  fireEvent.click(back!);
  expect(screen.getByRole('button', { name: 'Continue' })).toBeInTheDocument();
  fixture.unmount();
  const next = await fixture.transactions.acquireWalletSendLock('wallet');
  next.release();
  expect(reset).toHaveBeenCalledTimes(1);
  await act(async () => {
    transport.resolve(null);
  });
  expect(fixture.submit).not.toHaveBeenCalled();
});

it.each(['quit', 'join'] as const)(
  '%s unmount while queued invalidates selection before it can reach the backend',
  async (flow) => {
    // Occupy the same queue before opening the legacy hardware operation.
    const device = jest
      .spyOn(getHardwareWalletTransportChannel, 'request')
      .mockResolvedValue(null);
    const fixture = mountFlow('join');
    const blocker = await fixture.transactions.acquireWalletSendLock('wallet');
    if (flow === 'join') await reachConfirmation();
    else {
      act(() => {
        fixture.actions.dialogs.closeActiveDialog.trigger();
        fixture.actions.dialogs.open.trigger({
          dialog: UndelegateWalletDialogContainer as unknown as Parameters<
            DialogsActions['open']['trigger']
          >[0]['dialog'],
        });
        fixture.actions.dialogs.updateDataForActiveDialog.trigger({
          data: { walletId: 'wallet' },
        });
      });
    }
    fixture.unmount();
    blocker.release();
    const next = await fixture.transactions.acquireWalletSendLock('wallet');
    next.release();
    expect(fixture.select).not.toHaveBeenCalled();
    expect(device).not.toHaveBeenCalled();
  }
);

it.each(['quit', 'join'] as const)(
  'pilot %s leaves unified hardware approval ownership alone on unmount',
  async (flow) => {
    global.cip30Enabled = true;
    const fixture = mountFlow(flow);
    await fixture.hardwareWallets.selectDelegationCoins({
      walletId: 'wallet',
      poolId: 'new-pool',
      delegationAction: 'join',
    });
    if (flow === 'join') {
      fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
      fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
      fireEvent.click(await screen.findByRole('button', { name: 'Continue' }));
    }
    fixture.unmount();
    let entered = false;
    const next = fixture.transactions.withWalletSendLock('wallet', async () => {
      entered = true;
    });
    await act(async () => {
      await Promise.resolve();
    });
    expect(entered).toBe(false);
    await fixture.hardwareWallets._resetTransaction({
      cancelDeviceAction: false,
    });
    await next;
    expect(entered).toBe(true);
  }
);

it.each(['quit', 'join'] as const)(
  '%s unmount cannot release an already submitting external transaction',
  async (flow) => {
    const transport = deferred<null>();
    const device = jest
      .spyOn(getHardwareWalletTransportChannel, 'request')
      .mockReturnValue(transport.promise);
    const fixture = mountFlow(flow);
    if (flow === 'join') await reachConfirmation();
    await waitFor(() => expect(device).toHaveBeenCalledTimes(1));
    const submission = deferred<{ id: string }>();
    fixture.submit.mockImplementation(() => submission.promise);
    runInAction(() => {
      fixture.hardwareWallets.signedTx = '84a0a0f5f6';
    });
    const sending = fixture.hardwareWallets._sendMoney({
      selectedWalletId: 'wallet',
      isDelegationTransaction: true,
    });
    await waitFor(() => expect(fixture.submit).toHaveBeenCalledTimes(1));
    fixture.unmount();
    let entered = false;
    const next = fixture.transactions.withWalletSendLock('wallet', async () => {
      entered = true;
    });
    await act(async () => {
      await Promise.resolve();
    });
    expect(entered).toBe(false);
    await act(async () => {
      submission.resolve({ id: 'transaction' });
      await sending;
      await next;
      transport.resolve(null);
    });
    expect(entered).toBe(true);
  }
);
