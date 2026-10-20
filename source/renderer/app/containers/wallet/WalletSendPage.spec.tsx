import React from 'react';
import BigNumber from 'bignumber.js';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
  within,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import { Provider } from 'mobx-react';
import { runInAction } from 'mobx';
import { TestDecorator } from '../../../../../tests/_utils/TestDecorator';
import WalletSendPage from './WalletSendPage';
import UiDialogsStore from '../../stores/UiDialogsStore';
import HardwareWalletsStore from '../../stores/HardwareWalletsStore';
import TransactionsStore from '../../stores/TransactionsStore';
import DialogsActions from '../../actions/dialogs-actions';
import Action from '../../actions/lib/Action';
import { noopAnalyticsTracker } from '../../analytics';
import { BrowserLocalStorageBridge } from '../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../features/discreet-mode';
import { NUMBER_OPTIONS } from '../../config/profileConfig';
import { HwDeviceStatuses } from '../../domains/Wallet';
import { DeviceTypes } from '../../../../common/types/hardware-wallets.types';
import {
  getHardwareWalletTransportChannel,
  resetTrezorActionChannel,
} from '../../ipc/getHardwareWalletChannel';
import type { Api } from '../../api';
import type { ActionsMap } from '../../actions';
import type { StoresMap } from '../../stores';
import type { CoinSelectionsResponse } from '../../api/transactions/types';

jest.mock('../../components/analytics/withAnalytics', () => ({
  withAnalytics: (component: React.ComponentType) => component,
}));
jest.mock('@trezor/connect', () => ({}));
jest.mock('lodash-es/util', () => ({
  times: jest.requireActual('lodash/times'),
}));
jest.mock('../../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

const receiver =
  'addr1qyf5s3w0n3l4nywnqeyyvvs8jnkg9nn2k4exsldnqx7xgk6vr3p9s9xsd9xswykg5f8dkag5vn8f3wfzfhg6y3g5f7sqh3r8zt';
const originalPilot = global.cip30Enabled;
const originalFlight = global.isFlight;
let dispose: () => Promise<void> = async () => undefined;

function mountSend(isHardwareWallet: boolean, isLegacy = false) {
  const wallet = {
    id: 'wallet',
    name: 'Ledger smoke wallet',
    isHardwareWallet,
    isLegacy,
    amount: new BigNumber(20),
    availableAmount: new BigNumber(20),
    reward: new BigNumber(0),
    assets: { total: [] },
    isRestoring: false,
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
  const submitExternal = jest.fn();
  const api = {
    ada: { selectCoins, createExternalTransaction: submitExternal },
    localStorage: {},
  } as unknown as Api;
  const softwareSend = jest.fn();
  const actions = {
    dialogs: new DialogsActions(),
    assets: { onCopyAssetParam: new Action() },
    wallets: {
      unsetActiveAsset: new Action(),
      sendMoney: { trigger: softwareSend },
    },
    hardwareWallets: { sendMoney: new Action() },
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
  const stores = {
    uiDialogs,
    hardwareWallets,
    wallets: {
      active: wallet,
      getWalletById: () => wallet,
      isValidAddress: () => true,
      isAddressFromSameWallet: true,
      sendMoneyRequest: { isExecuting: false, error: null, reset: jest.fn() },
      refreshWalletsData: jest.fn(),
    },
    transactions: {
      acquireWalletSendLock: transactions.acquireWalletSendLock,
      validateAmount: async () => true,
      validateAssetAmount: async () => true,
      calculateTransactionFee: async () => ({ fee: new BigNumber('0.17') }),
    },
    app: { openExternalLink: jest.fn() },
    profile: { currentNumberFormat: NUMBER_OPTIONS[0].value },
    assets: { all: [], getAsset: jest.fn(), favorites: {} },
    addresses: { all: [] },
    collateral: { preparationFormActive: false },
  } as unknown as StoresMap;
  uiDialogs.configure(stores);
  uiDialogs.setup();
  hardwareWallets.configure(stores);
  actions.hardwareWallets.sendMoney.listen(hardwareWallets._sendMoney);
  runInAction(() => {
    hardwareWallets.hardwareWalletsLocalDataRequest.result = {
      wallet: {
        id: 'ledger',
        disconnected: false,
        device: { deviceType: DeviceTypes.LEDGER, path: 'fixture-ledger' },
      },
    } as unknown as HardwareWalletsStore['hardwareWalletsConnectionData'];
  });
  dispose = async () => {
    clearInterval(uiDialogs._secondsTimerInterval);
    clearInterval(hardwareWallets.checkTransactionTimeInterval);
    await hardwareWallets._resetTransaction({ cancelDeviceAction: false });
    actions.dialogs.open.removeAll();
    actions.dialogs.closeActiveDialog.removeAll();
    actions.dialogs.resetActiveDialog.removeAll();
    actions.dialogs.updateDataForActiveDialog.removeAll();
    actions.hardwareWallets.sendMoney.removeAll();
  };
  const view = render(
    <TestDecorator>
      <BrowserLocalStorageBridge>
        <DiscreetModeFeatureProvider>
          <Provider stores={stores} actions={actions}>
            <WalletSendPage
              {...({
                stores,
                actions,
                analyticsTracker: noopAnalyticsTracker,
              } as unknown as React.ComponentProps<typeof WalletSendPage>)}
            />
          </Provider>
        </DiscreetModeFeatureProvider>
      </BrowserLocalStorageBridge>
    </TestDecorator>
  );
  return {
    ...view,
    selection,
    hardwareWallets,
    transactions,
    selectCoins,
    submitExternal,
    softwareSend,
  };
}

async function clickSend() {
  fireEvent.change(screen.getByPlaceholderText('Paste an address'), {
    target: { value: receiver },
  });
  fireEvent.change(await screen.findByLabelText('Ada'), {
    target: { value: '5' },
  });
  const send = screen.getByRole('button', { name: 'Send' });
  await waitFor(() => expect(send).not.toBeDisabled(), { timeout: 5000 });
  fireEvent.click(send);
}

afterEach(async () => {
  cleanup();
  await dispose();
  jest.restoreAllMocks();
  global.cip30Enabled = originalPilot;
  global.isFlight = originalFlight;
});

it.each(['Back', 'unmount'])(
  'releases mainnet Ledger confirmation on %s without submission',
  async (exit) => {
    global.cip30Enabled = false;
    global.isFlight = false;
    let finishTransport!: () => void;
    const transport = jest
      .spyOn(getHardwareWalletTransportChannel, 'request')
      .mockImplementation(
        () =>
          new Promise((resolve) => {
            finishTransport = () => resolve(null);
          })
      );
    jest
      .spyOn(resetTrezorActionChannel, 'request')
      .mockResolvedValue(undefined);
    const fixture = mountSend(true);
    await clickSend();
    const dialog = await screen.findByLabelText('Modal Dialog');
    expect(dialog).toHaveTextContent('5.000000 ADA');
    expect(dialog).toHaveTextContent('5.170000 ADA');
    await waitFor(() => expect(transport).toHaveBeenCalledTimes(1));
    expect(within(dialog).getByRole('button', { name: 'Send' })).toBeDisabled();
    act(() => {
      fixture.hardwareWallets.hwDeviceStatus =
        HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
    });
    expect(
      within(dialog).getByRole('button', { name: 'Send' })
    ).not.toBeDisabled();
    act(() => {
      fixture.hardwareWallets.hwDeviceStatus =
        HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;
    });
    expect(within(dialog).getByRole('button', { name: 'Send' })).toBeDisabled();
    if (exit === 'Back')
      fireEvent.click(within(dialog).getByRole('button', { name: 'Back' }));
    else fixture.unmount();
    await waitFor(() =>
      expect(screen.queryByLabelText('Modal Dialog')).not.toBeInTheDocument()
    );
    await act(async () => {
      finishTransport();
    });
    const next = await fixture.transactions.acquireWalletSendLock('wallet');
    next.release();
    expect(fixture.submitExternal).not.toHaveBeenCalled();
    expect(fixture.softwareSend).not.toHaveBeenCalled();
    expect(transport).toHaveBeenCalledTimes(1);
  }
);

it('invalidates pending hardware selection when the Send page unmounts and releases the wallet queue', async () => {
  global.cip30Enabled = false;
  global.isFlight = false;
  const transport = jest
    .spyOn(getHardwareWalletTransportChannel, 'request')
    .mockResolvedValue(null);
  jest.spyOn(resetTrezorActionChannel, 'request').mockResolvedValue(undefined);
  const fixture = mountSend(true);
  let finishSelection!: (selection: CoinSelectionsResponse) => void;
  fixture.selectCoins
    .mockResolvedValueOnce(fixture.selection)
    .mockImplementationOnce(
      () =>
        new Promise<CoinSelectionsResponse>((resolve) => {
          finishSelection = resolve;
        })
    );
  await clickSend();
  await waitFor(() => expect(fixture.selectCoins).toHaveBeenCalledTimes(2));
  fixture.unmount();
  const next = await fixture.transactions.acquireWalletSendLock('wallet');
  next.release();
  const newerDraft = { ...fixture.selection, fee: new BigNumber('0.25') };
  fixture.hardwareWallets.updateTxSignRequest(newerDraft);
  await act(async () => {
    finishSelection(fixture.selection);
    await fixture.hardwareWallets.selectCoinsRequest.promise;
  });
  expect(fixture.hardwareWallets.txSignRequest.coinSelection.fee).toEqual(
    newerDraft.fee
  );
  expect(fixture.hardwareWallets.isTransactionInitiated).toBe(false);
  expect(transport).not.toHaveBeenCalled();
  expect(fixture.submitExternal).not.toHaveBeenCalled();
});

it('keeps an authorized hardware submission locked after its Send page unmounts until the backend settles', async () => {
  global.cip30Enabled = false;
  global.isFlight = false;
  let finishTransport!: () => void;
  const transport = jest
    .spyOn(getHardwareWalletTransportChannel, 'request')
    .mockImplementation(
      () =>
        new Promise((resolve) => {
          finishTransport = () => resolve(null);
        })
    );
  jest.spyOn(resetTrezorActionChannel, 'request').mockResolvedValue(undefined);
  const fixture = mountSend(true);
  let finishSubmission!: (result: { id: string }) => void;
  fixture.submitExternal.mockImplementation(
    () =>
      new Promise<{ id: string }>((resolve) => {
        finishSubmission = resolve;
      })
  );
  await clickSend();
  await waitFor(() => expect(transport).toHaveBeenCalledTimes(1));
  act(() => {
    fixture.hardwareWallets.signedTx = 'fixture-signed-transaction';
    fixture.hardwareWallets.hwDeviceStatus =
      HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
  });
  fireEvent.click(
    within(screen.getByLabelText('Modal Dialog')).getByRole('button', {
      name: 'Send',
    })
  );
  await waitFor(() =>
    expect(fixture.submitExternal).toHaveBeenCalledWith({
      signedTransactionBlob: 'fixture-signed-transaction',
    })
  );
  let entered = false;
  const nextOperation = fixture.transactions
    .acquireWalletSendLock('wallet')
    .then((lease) => {
      entered = true;
      lease.release();
    });
  fixture.unmount();
  await act(async () => {
    finishTransport();
  });
  expect(entered).toBe(false);
  await act(async () => {
    finishSubmission({ id: 'fixture-transaction' });
    await nextOperation;
  });
  expect(entered).toBe(true);
  expect(fixture.submitExternal).toHaveBeenCalledTimes(1);
  expect(fixture.softwareSend).not.toHaveBeenCalled();
});

it.each([
  [false, false],
  [false, true],
  [true, true],
])(
  'shows password approval for software Send (pilot=%s, legacy=%s)',
  async (pilot, legacy) => {
    global.cip30Enabled = pilot;
    global.isFlight = false;
    const fixture = mountSend(false, legacy);
    await clickSend();
    const dialog = await screen.findByLabelText('Modal Dialog');
    const approve = within(dialog).getByRole('button', { name: 'Send' });
    expect(approve).toBeDisabled();
    fireEvent.change(dialog.querySelector('input[type="password"]')!, {
      target: { value: 'fixture-password' },
    });
    await waitFor(() => expect(approve).not.toBeDisabled());
    fireEvent.click(approve);
    await waitFor(() =>
      expect(fixture.softwareSend).toHaveBeenCalledWith(
        expect.objectContaining({
          amount: '5000000',
          receiver,
          passphrase: 'fixture-password',
          isHardwareWallet: false,
        })
      )
    );
  }
);

it('keeps pilot Shelley Send on unified approval instead of the legacy dialog', async () => {
  global.cip30Enabled = true;
  global.isFlight = false;
  const fixture = mountSend(false);
  await clickSend();
  await waitFor(() =>
    expect(fixture.softwareSend).toHaveBeenCalledWith(
      expect.objectContaining({ amount: '5000000', receiver })
    )
  );
  expect(screen.queryByLabelText('Modal Dialog')).not.toBeInTheDocument();
});
