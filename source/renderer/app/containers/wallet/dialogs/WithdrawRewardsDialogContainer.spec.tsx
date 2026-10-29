import React from 'react';
import BigNumber from 'bignumber.js';
import { observable, runInAction } from 'mobx';
import { Provider } from 'mobx-react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import { messages as apiErrorMessages } from '../../../api/errors';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import type { RewardWithdrawalStep } from '../../../stores/WalletsStore';
import WithdrawRewardsDialogContainer from './WithdrawRewardsDialogContainer';

jest.mock('../../../components/widgets/Dialog', () => ({
  __esModule: true,
  default: ({
    children,
    actions,
  }: {
    children: React.ReactNode;
    actions: Array<{
      label: React.ReactNode;
      onClick: () => void;
      disabled?: boolean;
    }>;
  }) => (
    <div>
      {children}
      {actions.map((action, index) => (
        <button key={index} onClick={action.onClick} disabled={action.disabled}>
          {action.label}
        </button>
      ))}
    </div>
  ),
}));

const READY: RewardWithdrawalStep = {
  status: 'ready',
  amount: new BigNumber('36.5'),
  fee: new BigNumber('0.171793'),
};

const mount = ({
  isHardwareWallet,
  outcome,
  initiateError,
}: {
  isHardwareWallet: boolean;
  outcome: RewardWithdrawalStep;
  initiateError?: Error;
}) => {
  const state = observable.box<RewardWithdrawalStep | null>(null);
  const wallets = {
    getWalletById: () => ({
      id: 'wallet',
      name: 'Wallet',
      isHardwareWallet,
    }),
    rewardWithdrawalFor: () => state.get(),
    prepareRewardWithdrawal: jest.fn(async () => {
      runInAction(() => state.set(outcome));
    }),
    submitRewardWithdrawal: jest.fn(async () => undefined),
    cancelRewardWithdrawal: jest.fn(() => runInAction(() => state.set(null))),
    // The store maps an error without a message id to the internal error.
    _setRewardWithdrawal: jest.fn(
      (_id: string, step: RewardWithdrawalStep | null) =>
        runInAction(() =>
          state.set(
            step &&
              (step.status === 'failed' || step.status === 'ready') &&
              step.error != null &&
              typeof step.error.id !== 'string'
              ? {
                  ...step,
                  error: apiErrorMessages.walletInternalError as never,
                }
              : step
          )
        )
    ),
  };
  const hardwareWallets = observable({
    hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
    checkIsTrezorByWalletId: () => false,
    initiateTransaction: jest.fn(async () => {
      if (initiateError) throw initiateError;
    }),
    _resetTransaction: jest.fn(),
    sendMoneyRequest: { isExecuting: false },
    isTransactionPending: false,
  });
  const actions = {
    dialogs: { closeActiveDialog: { trigger: jest.fn() } },
    hardwareWallets: { sendMoney: { trigger: jest.fn(async () => undefined) } },
  };
  const stores = { wallets, hardwareWallets };
  const view = render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <Provider stores={stores} actions={actions}>
              <WithdrawRewardsDialogContainer
                walletId="wallet"
                onExternalLinkClick={jest.fn()}
              />
            </Provider>
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </IntlProvider>
    </ThemeProvider>
  );
  return { ...view, wallets, hardwareWallets, actions };
};

describe('WithdrawRewardsDialogContainer', () => {
  afterEach(cleanup);

  describe('on a software wallet', () => {
    it('prepares on mount, shows what it prepared and submits with the password', async () => {
      const { wallets, hardwareWallets } = mount({
        isHardwareWallet: false,
        outcome: READY,
      });
      expect(wallets.prepareRewardWithdrawal).toHaveBeenCalledWith('wallet');
      expect(await screen.findByText(/36\.500000 ADA/)).toBeInTheDocument();
      expect(hardwareWallets.initiateTransaction).not.toHaveBeenCalled();
      fireEvent.change(
        screen.getByPlaceholderText('Type your spending password'),
        { target: { value: 'fixture-password' } }
      );
      const submit = screen.getByRole('button', { name: 'Withdraw' });
      await waitFor(() => expect(submit).not.toBeDisabled());
      fireEvent.click(submit);
      await waitFor(() =>
        expect(wallets.submitRewardWithdrawal).toHaveBeenCalledWith(
          'wallet',
          'fixture-password'
        )
      );
    });

    it('cancels without a trace and closes', async () => {
      const { wallets, hardwareWallets, actions } = mount({
        isHardwareWallet: false,
        outcome: READY,
      });
      await screen.findByText(/36\.500000 ADA/);
      fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
      expect(wallets.cancelRewardWithdrawal).toHaveBeenCalledWith('wallet');
      expect(actions.dialogs.closeActiveDialog.trigger).toHaveBeenCalled();
      expect(hardwareWallets._resetTransaction).not.toHaveBeenCalled();
    });

    it('shows why nothing was sent when the wallet left the rewards out', async () => {
      mount({
        isHardwareWallet: false,
        outcome: {
          status: 'failed',
          error: apiErrorMessages.rewardWithdrawalNotBuilt as never,
        },
      });
      expect(await screen.findByRole('alert')).toHaveTextContent(
        /so nothing was sent/
      );
      expect(
        screen.queryByRole('button', { name: 'Withdraw' })
      ).not.toBeInTheDocument();
    });
  });

  describe('on a hardware wallet', () => {
    it('starts the device flow for the selection and submits through the hardware store', async () => {
      const { wallets, hardwareWallets, actions } = mount({
        isHardwareWallet: true,
        outcome: READY,
      });
      await waitFor(() =>
        expect(hardwareWallets.initiateTransaction).toHaveBeenCalledWith({
          walletId: 'wallet',
        })
      );
      fireEvent.click(await screen.findByRole('button', { name: 'Withdraw' }));
      await waitFor(() =>
        expect(actions.hardwareWallets.sendMoney.trigger).toHaveBeenCalledTimes(
          1
        )
      );
      expect(wallets.submitRewardWithdrawal).not.toHaveBeenCalled();
    });

    it('cannot be submitted again or cancelled while the transaction is pending', async () => {
      const { hardwareWallets, actions } = mount({
        isHardwareWallet: true,
        outcome: READY,
      });
      const submit = await screen.findByRole('button', { name: 'Withdraw' });
      fireEvent.click(submit);
      await waitFor(() =>
        expect(actions.hardwareWallets.sendMoney.trigger).toHaveBeenCalledTimes(
          1
        )
      );
      act(() => {
        runInAction(() => {
          hardwareWallets.isTransactionPending = true;
        });
      });
      expect(screen.getByRole('button', { name: 'Withdraw' })).toBeDisabled();
      expect(
        screen.queryByRole('button', { name: 'Cancel' })
      ).not.toBeInTheDocument();
      fireEvent.click(screen.getByRole('button', { name: 'Withdraw' }));
      expect(actions.hardwareWallets.sendMoney.trigger).toHaveBeenCalledTimes(
        1
      );
    });

    it('draws an internal error when the device flow rejects with a plain Error', async () => {
      const { hardwareWallets } = mount({
        isHardwareWallet: true,
        outcome: READY,
        initiateError: new Error('Signing device not recognized!'),
      });
      expect(await screen.findByRole('alert')).toHaveTextContent(
        /internal error/i
      );
      expect(hardwareWallets.initiateTransaction).toHaveBeenCalledWith({
        walletId: 'wallet',
      });
    });

    it('starts no device flow when the wallet left the rewards out', async () => {
      const { hardwareWallets } = mount({
        isHardwareWallet: true,
        outcome: {
          status: 'failed',
          error: apiErrorMessages.rewardWithdrawalNotBuilt as never,
        },
      });
      await screen.findByRole('alert');
      expect(hardwareWallets.initiateTransaction).not.toHaveBeenCalled();
    });

    it('releases the device transaction and the step on cancel and on unmount', async () => {
      const { wallets, hardwareWallets, unmount } = mount({
        isHardwareWallet: true,
        outcome: READY,
      });
      await screen.findByText(/36\.500000 ADA/);
      fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
      expect(hardwareWallets._resetTransaction).toHaveBeenCalledWith({
        cancelDeviceAction: true,
      });
      unmount();
      expect(wallets.cancelRewardWithdrawal).toHaveBeenCalledWith('wallet');
    });
  });
});
