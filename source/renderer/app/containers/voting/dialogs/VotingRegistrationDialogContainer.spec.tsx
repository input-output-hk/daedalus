import React from 'react';
import BigNumber from 'bignumber.js';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import { observer, Provider } from 'mobx-react';
import { runInAction } from 'mobx';
import { TestDecorator } from '../../../../../../tests/_utils/TestDecorator';
import VotingRegistrationDialogContainer from './VotingRegistrationDialogContainer';
import VotingRegistrationDialog from '../../../components/voting/voting-registration-wizard-steps/widgets/VotingRegistrationDialog';
import UiDialogsStore from '../../../stores/UiDialogsStore';
import HardwareWalletsStore from '../../../stores/HardwareWalletsStore';
import TransactionsStore from '../../../stores/TransactionsStore';
import VotingStore from '../../../stores/VotingStore';
import NativeTransactionService from '../../../services/NativeTransactionService';
import DialogsActions from '../../../actions/dialogs-actions';
import VotingActions from '../../../actions/voting-actions';
import { noopAnalyticsTracker } from '../../../analytics';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import { NUMBER_OPTIONS } from '../../../config/profileConfig';
import { DeviceTypes } from '../../../../../common/types/hardware-wallets.types';
import type {
  HardwareTransactionPreparation,
  TrezorDevicePayload,
} from '../../../../../common/types/hardware-wallets.types';
import {
  getHardwareWalletTransportChannel,
  resetTrezorActionChannel,
  signExactHardwareTransactionChannel,
} from '../../../ipc/getHardwareWalletChannel';
import type { Api } from '../../../api';
import type { ActionsMap } from '../../../actions';
import type { StoresMap } from '../../../stores';
import type {
  CoinSelectionsResponse,
  VotingDataType,
} from '../../../api/transactions/types';
import LocalizableError from '../../../i18n/LocalizableError';
import { GenericApiError } from '../../../api/common/errors';

jest.mock('../../../components/analytics/withAnalytics', () => ({
  withAnalytics: (component: React.ComponentType) => component,
}));
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
// Cryptography is an external boundary; stores, requests, actions and UI are real.
jest.mock('../../../utils/walletUtils', () => ({
  __esModule: true,
  default: Promise.resolve({
    Ed25519ExtendedPrivate: {
      generate: () => ({
        bytes: () => new Uint8Array([1]),
        public: () => ({ bytes: () => new Uint8Array([2]) }),
      }),
    },
    bech32_decode_to_bytes: (value: string) =>
      new Uint8Array(Buffer.from(value)),
  }),
}));

type Deferred<T> = {
  promise: Promise<T>;
  resolve: (value: T) => void;
  reject: (error: Error) => void;
};
// This project targets ES2019, which does not provide Promise.withResolvers.
const deferred = <T,>(): Deferred<T> => {
  let resolve!: (value: T) => void;
  let reject!: (error: Error) => void;
  const promise = new Promise<T>((complete, fail) => {
    resolve = complete;
    reject = fail;
  });
  return { promise, resolve, reject };
};
const originalPilot = global.cip30Enabled;
let dispose = () => {};

function mountRegistration(hardware = true, pilot = false) {
  global.cip30Enabled = pilot;
  const wallet = {
    id: 'wallet',
    name: 'Registration wallet',
    isHardwareWallet: hardware,
    isLegacy: false,
    isRestoring: false,
    amount: new BigNumber(1000),
    availableAmount: new BigNumber(1000),
    reward: new BigNumber(0),
    assets: { total: [] },
  };
  const selection: CoinSelectionsResponse = {
    inputs: [],
    outputs: [],
    certificates: [],
    withdrawals: [],
    metadata: null,
    deposits: new BigNumber(0),
    depositsReclaimed: new BigNumber(0),
    fee: new BigNumber('0.17'),
  };
  const selectCoins = jest.fn().mockResolvedValue(selection);
  const constructTransaction = jest.fn().mockResolvedValue({
    transaction: 'a100',
    coinSelection: selection,
  });
  const publicKey = jest.fn().mockResolvedValue('stake-key');
  const externalSubmit = jest.fn();
  const softwareSubmit = jest.fn();
  const calculateFee = jest
    .fn()
    .mockResolvedValue({ fee: new BigNumber('0.17') });
  const api = {
    ada: {
      selectCoins,
      getWalletPublicKey: publicKey,
      constructTransaction,
      createExternalTransaction: externalSubmit,
      createVotingRegistrationTransaction: softwareSubmit,
      calculateTransactionFee: calculateFee,
      getCatalystFund: () => new Promise(() => {}),
    },
    localStorage: {},
  } as unknown as Api;
  const actions = {
    dialogs: new DialogsActions(),
    voting: new VotingActions(),
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
  const voting = new VotingStore(api, actions, noopAnalyticsTracker);
  const nativeTransactions = new NativeTransactionService(
    transactions.withWalletSendLock
  );
  const nativeRun = jest.spyOn(nativeTransactions, 'run');
  const getAddressesByWalletId = jest
    .fn()
    .mockResolvedValue([{ id: 'upstream-address' }]);
  const getAutomaticReceivingAddress = jest
    .fn()
    .mockResolvedValue({ id: 'pilot-address' });
  const stores = {
    uiDialogs,
    hardwareWallets,
    voting,
    wallets: {
      all: [wallet],
      active: wallet,
      getWalletById: (walletId: string) => ({ ...wallet, id: walletId }),
      refreshWalletsData: jest.fn(),
    },
    transactions,
    walletApproval: { nativeTransactions },
    addresses: { getAddressesByWalletId, getAutomaticReceivingAddress },
    networkStatus: { absoluteSlotNumber: 123 },
    staking: { stakePools: [], getStakePoolById: jest.fn() },
    app: { openExternalLink: jest.fn() },
    profile: { currentNumberFormat: NUMBER_OPTIONS[0].value },
    assets: { all: [], getAsset: jest.fn(), favorites: {} },
  } as unknown as StoresMap;
  uiDialogs.configure(stores);
  uiDialogs.setup();
  transactions.configure(stores);
  hardwareWallets.configure(stores);
  voting.configure(stores);
  voting.setup();
  runInAction(() => {
    // Preselect so the real wallet dropdown need not be driven by incidental markup.
    voting.selectedWalletId = wallet.id;
    hardwareWallets.hardwareWalletsLocalDataRequest.result = {
      wallet: {
        id: 'ledger',
        disconnected: false,
        device: { deviceType: DeviceTypes.LEDGER, path: 'fixture-ledger' },
      },
      'other-wallet': {
        id: 'other-ledger',
        disconnected: false,
        device: {
          deviceType: DeviceTypes.LEDGER,
          path: 'other-fixture-ledger',
        },
      },
    } as unknown as HardwareWalletsStore['hardwareWalletsConnectionData'];
  });
  const transports: Array<Deferred<null>> = [];
  const transport = jest
    .spyOn(getHardwareWalletTransportChannel, 'request')
    .mockImplementation(() => {
      const pending = deferred<null>();
      transports.push(pending);
      return pending.promise;
    });
  const reset = jest
    .spyOn(resetTrezorActionChannel, 'request')
    .mockResolvedValue(undefined);
  const Host = observer(() =>
    uiDialogs.isOpen(VotingRegistrationDialog) ? (
      <VotingRegistrationDialogContainer />
    ) : null
  );
  actions.dialogs.open.trigger({
    // Legacy actions type class-component identifiers as functions.
    dialog: VotingRegistrationDialog as unknown as Parameters<
      DialogsActions['open']['trigger']
    >[0]['dialog'],
  });
  const mounted = render(
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
    nativeTransactions.dispose();
    clearInterval(hardwareWallets.checkTransactionTimeInterval);
    clearInterval(uiDialogs._secondsTimerInterval);
    Object.values(actions.voting).forEach((action) => action.removeAll());
    Object.values(actions.dialogs).forEach((action) => action.removeAll());
  };
  const start = () =>
    fireEvent.click(screen.getByRole('button', { name: 'Continue' }));
  const close = () => {
    // The legacy close control has no accessible label.
    const closeButton = screen
      .getByLabelText('Modal Dialog')
      .querySelector('button[tabindex="-1"]');
    expect(closeButton).not.toBeNull();
    fireEvent.click(closeButton!);
  };
  const back = () => {
    // DialogBackButton is the last title-bar control and has no accessible label.
    const buttons = screen
      .getByLabelText('Modal Dialog')
      .querySelectorAll('button');
    fireEvent.click(buttons[buttons.length - 1]);
  };
  return {
    ...mounted,
    start,
    close,
    back,
    actions,
    uiDialogs,
    voting,
    hardwareWallets,
    transactions,
    nativeRun,
    nativeTransactions,
    selectCoins,
    constructTransaction,
    selection,
    publicKey,
    transport,
    transports,
    reset,
    externalSubmit,
    softwareSubmit,
    calculateFee,
    getAddressesByWalletId,
    getAutomaticReceivingAddress,
  };
}

interface RegistrationFixture {
  start: () => void;
  back: () => void;
  close: () => void;
  unmount: () => void;
  transactions: TransactionsStore;
  hardwareWallets: HardwareWalletsStore;
  getAutomaticReceivingAddress: jest.Mock;
  publicKey: jest.Mock;
  constructTransaction: jest.Mock;
  selection: CoinSelectionsResponse;
}
type Exit = 'Back' | 'confirmed Cancel' | 'unmount';
type PreparationPhase = 'automatic address' | 'public key' | 'preview';

function leaveRegistration(fixture: RegistrationFixture, exit: Exit) {
  if (exit === 'Back') fixture.back();
  else if (exit === 'unmount') fixture.unmount();
  else {
    fixture.close();
    fireEvent.click(
      screen.getByRole('button', { name: 'Cancel registration' })
    );
  }
}

async function pauseRegistration(
  fixture: RegistrationFixture,
  phase: PreparationPhase
) {
  const resumed = deferred<void>();
  let finish: () => void;
  let entered: jest.Mock;
  if (phase === 'automatic address') {
    const address = deferred<{ id: string }>();
    fixture.getAutomaticReceivingAddress.mockImplementationOnce(
      () => address.promise
    );
    entered = fixture.getAutomaticReceivingAddress;
    finish = () => {
      address.resolve({ id: 'late-pilot-address' });
      address.promise.then(() => resumed.resolve());
    };
  } else if (phase === 'public key') {
    const key = deferred<string>();
    fixture.publicKey.mockImplementationOnce(() => key.promise);
    entered = fixture.publicKey;
    finish = () => {
      key.resolve('late-stake-key');
      key.promise.then(() => resumed.resolve());
    };
  } else {
    const preview = deferred<{
      transaction: string;
      coinSelection: CoinSelectionsResponse;
    }>();
    fixture.constructTransaction.mockImplementationOnce(() => preview.promise);
    entered = fixture.constructTransaction;
    finish = () => {
      preview.resolve({
        transaction: 'a100',
        coinSelection: fixture.selection,
      });
      preview.promise.then(() => resumed.resolve());
    };
  }
  const callsBefore = entered.mock.calls.length;
  fixture.start();
  await waitFor(() => expect(entered).toHaveBeenCalledTimes(callsBefore + 1));
  return async () => {
    await act(async () => {
      finish();
      await resumed.promise;
    });
  };
}

async function queueFollower(
  fixture: RegistrationFixture,
  walletId = 'wallet'
) {
  let entered = false;
  const completed = fixture.transactions
    .acquireWalletSendLock(walletId)
    .then((lease) => {
      entered = true;
      lease.release();
    });
  await act(async () => {
    await Promise.resolve();
  });
  return { completed, entered: () => entered };
}

async function beginNativePayment(
  fixture: ReturnType<typeof mountRegistration>,
  walletId: string
) {
  const preview = await fixture.hardwareWallets.selectCoins({
    walletId,
    address: 'unrelated-payment-address',
    amount: 1000000,
  });
  fixture.hardwareWallets.updateTxSignRequest(preview);
  const draft = fixture.hardwareWallets.txSignRequest;
  const construction = deferred<{
    transaction: string;
    coinSelection: CoinSelectionsResponse;
  }>();
  fixture.constructTransaction.mockImplementationOnce(
    () => construction.promise
  );
  const operation = fixture.hardwareWallets
    ._sendMoney({ selectedWalletId: walletId })
    .then(
      () => null,
      (error: Error) => error
    );
  await waitFor(() =>
    expect(fixture.constructTransaction).toHaveBeenCalledTimes(2)
  );
  const signal = fixture.nativeRun.mock.calls[0][0].ownerSignal;
  return { draft, construction, operation, signal };
}

const earlyCancellationCases: Array<[PreparationPhase, Exit, string]> = [
  ['automatic address', 'Back', 'wallet'],
  ['automatic address', 'confirmed Cancel', 'other-wallet'],
  ['automatic address', 'unmount', 'wallet'],
  ['public key', 'Back', 'other-wallet'],
  ['public key', 'confirmed Cancel', 'wallet'],
  ['public key', 'unmount', 'other-wallet'],
  ['preview', 'Back', 'wallet'],
  ['preview', 'confirmed Cancel', 'other-wallet'],
  ['preview', 'unmount', 'wallet'],
];

it.each(earlyCancellationCases)(
  '%s / %s preserves unrelated native hardware construction on %s',
  async (phase, exit, walletId) => {
    const fixture = mountRegistration(true, true);
    const { draft, construction, operation, signal } = await beginNativePayment(
      fixture,
      walletId
    );
    const follower = await queueFollower(fixture, walletId);
    expect(follower.entered()).toBe(false);
    const resume = await pauseRegistration(fixture, phase);
    leaveRegistration(fixture, exit);
    await resume();
    expect(signal.aborted).toBe(false);
    expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
    expect(follower.entered()).toBe(false);
    expect(fixture.transport).not.toHaveBeenCalled();
    expect(fixture.reset).not.toHaveBeenCalled();
    const failure = new Error('unrelated backend construction ended');
    await act(async () => {
      construction.reject(failure);
      expect(await operation).toBe(failure);
      await follower.completed;
    });
    expect(follower.entered()).toBe(true);
    expect(fixture.externalSubmit).not.toHaveBeenCalled();
  }
);

it('refuses to claim native hardware preparation owned by another wallet', async () => {
  const fixture = mountRegistration(true, true);
  const { draft, construction, operation, signal } = await beginNativePayment(
    fixture,
    'other-wallet'
  );
  const follower = await queueFollower(fixture, 'other-wallet');
  const refused = fixture.hardwareWallets.createVotingTransaction();
  await expect(
    refused.initiate({
      walletId: 'wallet',
      coinSelection: await paymentPreview(fixture),
      votingData: operationVotingData,
    })
  ).rejects.toBeInstanceOf(GenericApiError);
  refused.cancel();
  const freeWallet = await fixture.transactions.acquireWalletSendLock('wallet');
  freeWallet.release();
  expect(signal.aborted).toBe(false);
  expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
  expect(follower.entered()).toBe(false);
  expect(fixture.transport).not.toHaveBeenCalled();
  expect(fixture.reset).not.toHaveBeenCalled();
  const failure = new Error('unrelated backend construction ended');
  construction.reject(failure);
  expect(await operation).toBe(failure);
  await follower.completed;
});

const operationVotingData: VotingDataType = {
  address: { id: 'operation-address', spendingPath: '0/0' },
  stakeAddress: 'stake-address',
  stakeAddressHex: '00',
  votingKey: '01',
  stakeKey: '02',
  role: 'mutable_account',
  index: '0',
  metadata: {
    61284: { map: [{ k: { int: 1 }, v: { bytes: '01' } }] },
    61285: { map: [] },
  },
  nonce: 123,
};

async function paymentPreview(
  fixture: RegistrationFixture,
  walletId = 'wallet'
) {
  return fixture.hardwareWallets.selectCoins({
    walletId,
    address: 'payment-address',
    amount: 1000000,
  });
}

it.each(['wallet', 'other-wallet'])(
  'early Catalyst unmount preserves an initiated hardware lease for %s',
  async (walletId) => {
    const fixture = mountRegistration();
    fixture.hardwareWallets.updateTxSignRequest(
      await paymentPreview(fixture, walletId)
    );
    const initiating = fixture.hardwareWallets.initiateTransaction({
      walletId,
    });
    await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
    const draft = fixture.hardwareWallets.txSignRequest;
    const follower = await queueFollower(fixture, walletId);
    const resume = await pauseRegistration(fixture, 'public key');
    fixture.unmount();
    await resume();
    expect(fixture.hardwareWallets.isTransactionInitiated).toBe(true);
    expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
    expect(follower.entered()).toBe(false);
    expect(fixture.transport).toHaveBeenCalledTimes(1);
    expect(fixture.reset).not.toHaveBeenCalled();
    await fixture.hardwareWallets._resetTransaction({
      cancelDeviceAction: false,
    });
    await follower.completed;
    fixture.transports[0].resolve(null);
    await initiating;
  }
);

it('refuses to claim a different-wallet initiated lease after Catalyst preview completes', async () => {
  const fixture = mountRegistration();
  fixture.hardwareWallets.updateTxSignRequest(await paymentPreview(fixture));
  const initiating = fixture.hardwareWallets.initiateTransaction({
    walletId: 'wallet',
  });
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  const draft = fixture.hardwareWallets.txSignRequest;
  const follower = await queueFollower(fixture);
  const operation = fixture.hardwareWallets.createVotingTransaction();
  const preview = await fixture.hardwareWallets.selectCoins({
    walletId: 'other-wallet',
    address: 'catalyst-address',
    amount: 1000000,
  });
  await expect(
    operation.initiate({
      walletId: 'other-wallet',
      coinSelection: preview,
      votingData: operationVotingData,
    })
  ).rejects.toBeInstanceOf(GenericApiError);
  operation.cancel();
  const otherWallet =
    await fixture.transactions.acquireWalletSendLock('other-wallet');
  otherWallet.release();
  expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(true);
  expect(follower.entered()).toBe(false);
  expect(fixture.reset).not.toHaveBeenCalled();
  await fixture.hardwareWallets._resetTransaction({
    cancelDeviceAction: false,
  });
  await follower.completed;
  fixture.transports[0].resolve(null);
  await initiating;
});

it('cancelled queued acquisition releases only its late lease and a stale handle cannot reset its successor', async () => {
  const fixture = mountRegistration();
  const blocker = await fixture.transactions.acquireWalletSendLock('wallet');
  const preview = await paymentPreview(fixture);
  const old = fixture.hardwareWallets.createVotingTransaction();
  const retired = old.initiate({
    walletId: 'wallet',
    coinSelection: preview,
    votingData: operationVotingData,
  });
  old.cancel();
  const current = fixture.hardwareWallets.createVotingTransaction();
  const initiated = current.initiate({
    walletId: 'wallet',
    coinSelection: preview,
    votingData: operationVotingData,
  });
  blocker.release();
  await retired;
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  const draft = fixture.hardwareWallets.txSignRequest;
  const follower = await queueFollower(fixture);
  old.cancel();
  expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(true);
  expect(follower.entered()).toBe(false);
  expect(fixture.reset).not.toHaveBeenCalled();
  current.cancel();
  await follower.completed;
  const successorLease =
    await fixture.transactions.acquireWalletSendLock('wallet');
  const next = await queueFollower(fixture);
  current.cancel();
  old.cancel();
  expect(next.entered()).toBe(false);
  expect(fixture.reset).toHaveBeenCalledTimes(1);
  successorLease.release();
  await next.completed;
  fixture.transports[0].resolve(null);
  await initiated;
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(false);
  expect(fixture.externalSubmit).not.toHaveBeenCalled();
});

it('owned Catalyst teardown cannot release a detached lease while submission is pending', async () => {
  const fixture = mountRegistration();
  const operation = fixture.hardwareWallets.createVotingTransaction();
  const initiated = operation.initiate({
    walletId: 'wallet',
    coinSelection: await paymentPreview(fixture),
    votingData: operationVotingData,
  });
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  const backend = deferred<{ id: string }>();
  fixture.externalSubmit.mockImplementationOnce(() => backend.promise);
  // Seed the already-signed device result; this case exercises submission lifetime,
  // not cryptographic signing or physical-device approval.
  runInAction(() => {
    fixture.hardwareWallets.signedTx = '84a0a0f5f6';
  });
  const submission = fixture.hardwareWallets._sendMoney({
    selectedWalletId: 'wallet',
    isVotingRegistrationTransaction: true,
  });
  await waitFor(() => expect(fixture.externalSubmit).toHaveBeenCalledTimes(1));
  const follower = await queueFollower(fixture);
  operation.cancel();
  fixture.unmount();
  operation.cancel();
  expect(follower.entered()).toBe(false);
  backend.resolve({ id: 'submitted-catalyst' });
  await submission;
  await follower.completed;
  expect(follower.entered()).toBe(true);
  fixture.transports[0].resolve(null);
  await initiated;
  expect(fixture.externalSubmit).toHaveBeenCalledTimes(1);
});

it('scoped Catalyst cancellation cannot cancel an independently active exact Trezor signer', async () => {
  const fixture = mountRegistration(true, true);
  const operation = fixture.hardwareWallets.createVotingTransaction();
  const initiated = operation.initiate({
    walletId: 'wallet',
    coinSelection: await paymentPreview(fixture),
    votingData: operationVotingData,
  });
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  const device = {
    disconnected: false,
    deviceId: 'independent-trezor',
    deviceType: DeviceTypes.TREZOR,
    deviceModel: 'T',
    deviceName: 'Independent Trezor',
    path: 'independent-trezor-path',
    eventType: 'connect',
  } as unknown as TrezorDevicePayload;
  runInAction(() => {
    fixture.hardwareWallets.hardwareWalletsLocalDataRequest.result = {
      ...fixture.hardwareWallets.hardwareWalletsConnectionData,
      'signer-wallet': {
        id: 'signer-wallet',
        disconnected: false,
        device,
      },
    } as unknown as HardwareWalletsStore['hardwareWalletsConnectionData'];
    fixture.hardwareWallets.connectedHardwareWalletsDevices.set(
      device.path,
      device
    );
  });
  const deviceResult = deferred<{
    witnessSetCbor: string;
    signedTransactionCbor: string;
  }>();
  const sign = jest
    .spyOn(signExactHardwareTransactionChannel, 'request')
    .mockImplementation(() => deviceResult.promise);
  const exact = {
    status: 'ready',
    deviceInteraction: true,
    exact: {
      capability: {
        vendor: DeviceTypes.TREZOR,
        productEnabled: true,
        physicalCertified: true,
      },
    },
  } as unknown as HardwareTransactionPreparation;
  const signing = fixture.hardwareWallets.signDappTransaction(
    'signer-wallet',
    exact
  );
  await waitFor(() => expect(sign).toHaveBeenCalledTimes(1));
  const follower = await queueFollower(fixture);
  operation.cancel();
  await follower.completed;
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(false);
  expect(fixture.reset).not.toHaveBeenCalled();
  deviceResult.resolve({
    witnessSetCbor: 'a0',
    signedTransactionCbor: '84a0a0f5f6',
  });
  await expect(signing).resolves.toBe('a0');
  fixture.transports[0].resolve(null);
  await initiated;
});

afterEach(async () => {
  cleanup();
  dispose();
  jest.restoreAllMocks();
  global.cip30Enabled = originalPilot;
});

it('confirmed Cancel registration unmounts and releases the real wallet queue without submission', async () => {
  const fixture = mountRegistration();
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  let entered = false;
  const next = fixture.transactions
    .acquireWalletSendLock('wallet')
    .then((lease) => {
      entered = true;
      lease.release();
    });
  await act(async () => {
    await Promise.resolve();
  });
  expect(entered).toBe(false);
  fixture.close();
  expect(fixture.reset).not.toHaveBeenCalled();
  fireEvent.click(
    screen.getByRole('button', { name: 'Continue registration' })
  );
  expect(fixture.reset).not.toHaveBeenCalled();
  expect(entered).toBe(false);
  fixture.close();
  fireEvent.click(screen.getByRole('button', { name: 'Cancel registration' }));
  await act(async () => {
    await next;
  });
  expect(screen.queryByLabelText('Modal Dialog')).not.toBeInTheDocument();
  expect(entered).toBe(true);
  expect(fixture.reset).toHaveBeenCalledTimes(1);
  await act(async () => fixture.transports[0].resolve(null));
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(false);
  expect(fixture.externalSubmit).not.toHaveBeenCalled();
  expect(fixture.softwareSubmit).not.toHaveBeenCalled();
});

it('unmount releases an owned preparation exactly once', async () => {
  const fixture = mountRegistration();
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  fixture.unmount();
  const lease = await fixture.transactions.acquireWalletSendLock('wallet');
  lease.release();
  expect(fixture.reset).toHaveBeenCalledTimes(1);
  await act(async () => fixture.transports[0].resolve(null));
  expect(fixture.externalSubmit).not.toHaveBeenCalled();
});

it('Back releases preparation and permits a fresh selection without stale selection overwriting it', async () => {
  const fixture = mountRegistration();
  const oldSelection = deferred<CoinSelectionsResponse>();
  fixture.selectCoins.mockImplementationOnce(() => oldSelection.promise);
  fixture.start();
  await waitFor(() => expect(fixture.selectCoins).toHaveBeenCalledTimes(1));
  const retiredSelection = fixture.hardwareWallets.selectCoinsRequest.promise;
  fixture.back();
  expect(fixture.voting.registrationStep).toBe(1);
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  const currentDraft = fixture.hardwareWallets.txSignRequest;
  await act(async () => {
    oldSelection.resolve({ ...fixture.selection, fee: new BigNumber(9) });
    await retiredSelection;
  });
  expect(fixture.hardwareWallets.txSignRequest).toBe(currentDraft);
  expect(fixture.transport).toHaveBeenCalledTimes(1);
  expect(screen.getByLabelText('Modal Dialog')).not.toHaveTextContent(
    '9.000000'
  );
  fixture.unmount();
  const lease = await fixture.transactions.acquireWalletSendLock('wallet');
  lease.release();
  await act(async () => fixture.transports[0].resolve(null));
});

it('Restart invalidates pending metadata before a newer registration draft', async () => {
  const fixture = mountRegistration();
  const oldMetadata = deferred<string>();
  fixture.publicKey.mockImplementationOnce(() => oldMetadata.promise);
  fixture.start();
  await waitFor(() => expect(fixture.publicKey).toHaveBeenCalledTimes(1));
  const retiredMetadata = fixture.voting.getWalletPublicKeyRequest.promise;
  act(() => {
    fixture.hardwareWallets.sendMoneyRequest.error = new LocalizableError({
      id: 'test.device.rejected',
      defaultMessage: 'Device rejected registration',
    });
    fixture.actions.voting.nextRegistrationStep.trigger();
  });
  fireEvent.click(screen.getByRole('button', { name: /restart/i }));
  expect(fixture.voting.votingRegistrationKey).toBeNull();
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  const key = fixture.voting.votingRegistrationKey;
  const draft = fixture.hardwareWallets.txSignRequest;
  await act(async () => {
    oldMetadata.resolve('old-stake-key');
    await retiredMetadata;
  });
  expect(fixture.voting.votingRegistrationKey).toBe(key);
  expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
  expect(fixture.selectCoins).toHaveBeenCalledTimes(2);
  expect(fixture.transport).toHaveBeenCalledTimes(1);
  fixture.unmount();
  await act(async () => fixture.transports[0].resolve(null));
});

it.each([false, true])(
  'keeps software receiving behavior and ignores late fees for pilot=%s',
  async (pilot) => {
    const fixture = mountRegistration(false, pilot);
    const oldFee = deferred<{ fee: BigNumber }>();
    fixture.calculateFee.mockImplementationOnce(() => oldFee.promise);
    fixture.start();
    await waitFor(() => expect(fixture.calculateFee).toHaveBeenCalledTimes(1));
    fixture.back();
    fixture.start();
    await waitFor(() => expect(fixture.calculateFee).toHaveBeenCalledTimes(2));
    await act(async () => oldFee.resolve({ fee: new BigNumber(9) }));
    expect(fixture.calculateFee).toHaveBeenLastCalledWith(
      expect.objectContaining({
        walletId: 'wallet',
        address: pilot ? 'pilot-address' : 'upstream-address',
      })
    );
    expect(
      pilot
        ? fixture.getAddressesByWalletId
        : fixture.getAutomaticReceivingAddress
    ).not.toHaveBeenCalled();
    expect(screen.getByLabelText('Modal Dialog')).not.toHaveTextContent(
      '9.000000'
    );
    expect(fixture.transport).not.toHaveBeenCalled();
    expect(fixture.reset).not.toHaveBeenCalled();
  }
);

it('cancelled address lookup cannot start metadata or device preparation', async () => {
  const fixture = mountRegistration();
  const address = deferred<Array<{ id: string }>>();
  fixture.getAddressesByWalletId.mockImplementationOnce(() => address.promise);
  fixture.start();
  fixture.back();
  await act(async () => address.resolve([{ id: 'cancelled-address' }]));
  expect(fixture.publicKey).not.toHaveBeenCalled();
  expect(fixture.selectCoins).not.toHaveBeenCalled();
  expect(fixture.reset).not.toHaveBeenCalled();
  expect(fixture.transport).not.toHaveBeenCalled();
});

it('Back releases an active lease and a new registration can own the same wallet', async () => {
  const fixture = mountRegistration();
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  fixture.back();
  const next = await fixture.transactions.acquireWalletSendLock('wallet');
  next.release();
  expect(fixture.reset).toHaveBeenCalledTimes(1);
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(2));
  await act(async () => fixture.transports[0].resolve(null));
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(true);
  expect(fixture.hardwareWallets.activeVotingWalletId).toBe('wallet');
  fixture.unmount();
  await act(async () => fixture.transports[1].resolve(null));
  expect(fixture.externalSubmit).not.toHaveBeenCalled();
});

it('retains pilot receiving and Catalyst metadata through the real hardware selection', async () => {
  const fixture = mountRegistration(true, true);
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  expect(fixture.constructTransaction).toHaveBeenCalledWith({
    walletId: 'wallet',
    data: expect.objectContaining({
      payments: [expect.objectContaining({ address: 'pilot-address' })],
      metadata: expect.objectContaining({
        61284: expect.objectContaining({
          map: expect.arrayContaining([{ k: { int: 4 }, v: { int: 123 } }]),
        }),
      }),
    }),
  });
  expect(fixture.getAddressesByWalletId).not.toHaveBeenCalled();
  expect(fixture.selectCoins).not.toHaveBeenCalled();
  fixture.unmount();
  await act(async () => fixture.transports[0].resolve(null));
  expect(fixture.externalSubmit).not.toHaveBeenCalled();
});

it('does not replace a newer registration key after cancelled key generation resumes', async () => {
  const fixture = mountRegistration();
  let current = true;
  const pending = fixture.voting._generateVotingRegistrationKey(() => current);
  current = false;
  const newerKey = {
    bytes: () => new Uint8Array([8]),
    public: () => ({ bytes: () => new Uint8Array([9]) }),
  };
  fixture.voting._setVotingRegistrationKey(newerKey);
  const savedKey = fixture.voting.votingRegistrationKey;
  await expect(pending).rejects.toThrow('Voting registration cancelled.');
  expect(fixture.voting.votingRegistrationKey).toBe(savedKey);
});

it('Back does not reuse a cancelled metadata request or expose its late error', async () => {
  const fixture = mountRegistration();
  const oldMetadata = deferred<string>();
  fixture.publicKey.mockImplementationOnce(() => oldMetadata.promise);
  fixture.start();
  await waitFor(() => expect(fixture.publicKey).toHaveBeenCalledTimes(1));
  const retiredRequest = fixture.voting.getWalletPublicKeyRequest;
  fixture.back();
  fixture.start();
  await waitFor(() => expect(fixture.transport).toHaveBeenCalledTimes(1));
  expect(fixture.publicKey).toHaveBeenCalledTimes(2);
  const draft = fixture.hardwareWallets.txSignRequest;
  await act(async () =>
    oldMetadata.reject(
      new LocalizableError({
        id: 'test.cancelled.metadata',
        defaultMessage: 'Cancelled metadata failure',
      })
    )
  );
  // Request settles on its existing timer; wait for the retired request, not a sleep.
  await waitFor(() => expect(retiredRequest.isError).toBe(true));
  expect(fixture.voting.getWalletPublicKeyRequest.error).toBeNull();
  expect(fixture.hardwareWallets.txSignRequest).toBe(draft);
  expect(screen.getByLabelText('Modal Dialog')).not.toHaveTextContent(
    'Cancelled metadata failure'
  );
  fixture.unmount();
  await act(async () => fixture.transports[0].resolve(null));
});

it('prepares the newly selected wallet metadata without awaiting or reusing the cancelled wallet key request', async () => {
  const fixture = mountRegistration();
  const oldPublicKey = deferred<string>();
  fixture.publicKey.mockImplementationOnce(() => oldPublicKey.promise);
  let current = true;
  const oldPreparation = fixture.voting.prepareVotingData({
    walletId: 'old-wallet',
    isCurrent: () => current,
  });
  await waitFor(() =>
    expect(fixture.publicKey).toHaveBeenCalledWith({
      walletId: 'old-wallet',
      role: 'mutable_account',
      index: '0',
    })
  );
  current = false;
  fixture.publicKey.mockResolvedValueOnce('new-wallet-stake-key');
  const newPreparation = await fixture.voting.prepareVotingData({
    walletId: 'new-wallet',
    isCurrent: () => true,
  });
  const newKey = fixture.voting.votingRegistrationKey;
  expect(fixture.publicKey).toHaveBeenLastCalledWith({
    walletId: 'new-wallet',
    role: 'mutable_account',
    index: '0',
  });
  expect(newPreparation.stakeKey).toBe(
    Buffer.from('new-wallet-stake-key').toString('hex')
  );
  expect(newPreparation.metadata[61284].map).toContainEqual({
    k: { int: 2 },
    v: { bytes: newPreparation.stakeKey },
  });
  oldPublicKey.resolve('cancelled-wallet-stake-key');
  await expect(oldPreparation).rejects.toThrow(
    'Voting registration cancelled.'
  );
  expect(fixture.voting.votingRegistrationKey).toBe(newKey);
  expect(fixture.voting.getWalletPublicKeyRequest.result).toBe(
    'new-wallet-stake-key'
  );
});
