import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import {
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
import type { RewardWithdrawalStep } from '../../../stores/WalletsStore';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import {
  DiscreetModeFeatureProvider,
  DiscreetModeToggle,
} from '../../../features/discreet-mode';
import { AnalyticsProvider } from '../../analytics';
import { noopAnalyticsTracker } from '../../../analytics';
import WithdrawRewardsDialog from './WithdrawRewardsDialog';

jest.mock('../../widgets/Dialog', () => ({
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

const renderDialog = ({
  step,
  isHardwareWallet = false,
  hwDeviceStatus = HwDeviceStatuses.READY,
  isSubmitting,
}: {
  step: RewardWithdrawalStep | null;
  isHardwareWallet?: boolean;
  hwDeviceStatus?: string;
  isSubmitting?: boolean;
}) => {
  const handlers = {
    onSubmit: jest.fn(),
    onCancel: jest.fn(),
    onExternalLinkClick: jest.fn(),
  };
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <AnalyticsProvider tracker={noopAnalyticsTracker}>
          <BrowserLocalStorageBridge>
            <DiscreetModeFeatureProvider>
              <>
                <DiscreetModeToggle />
                <WithdrawRewardsDialog
                  walletName="Wallet"
                  isHardwareWallet={isHardwareWallet}
                  isSubmitting={isSubmitting}
                  step={step}
                  hwDeviceStatus={hwDeviceStatus as never}
                  isTrezor={false}
                  {...handlers}
                />
              </>
            </DiscreetModeFeatureProvider>
          </BrowserLocalStorageBridge>
        </AnalyticsProvider>
      </IntlProvider>
    </ThemeProvider>
  );
  return handlers;
};

const ready = (
  overrides: Partial<Extract<RewardWithdrawalStep, { status: 'ready' }>> = {}
): RewardWithdrawalStep => ({
  status: 'ready',
  amount: new BigNumber('36.5'),
  fee: new BigNumber('0.171793'),
  ...overrides,
});
const PASSWORD = 'Type your spending password';

describe('WithdrawRewardsDialog', () => {
  afterEach(cleanup);

  it('explains the transaction and waits for the amount while preparing', () => {
    renderDialog({ step: { status: 'preparing' } });
    expect(
      screen.getByText(
        "This transaction moves this wallet's staking rewards into the wallet and makes no payment."
      )
    ).toBeInTheDocument();
    expect(screen.getByText('Calculating fees')).toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Withdraw' })).toBeDisabled();
  });

  it('reads a wallet without a step as preparing', () => {
    renderDialog({ step: null });
    expect(screen.getByText('Calculating fees')).toBeInTheDocument();
  });

  it('shows the amount the wallet answered and the fee, and signs with the password', async () => {
    const handlers = renderDialog({ step: ready() });
    expect(screen.getByText('Staking rewards')).toBeInTheDocument();
    expect(screen.getByText(/36\.500000 ADA/)).toBeInTheDocument();
    expect(screen.getByText(/0\.171793 ADA/)).toBeInTheDocument();
    const submit = screen.getByRole('button', { name: 'Withdraw' });
    expect(submit).toBeDisabled();
    fireEvent.change(screen.getByPlaceholderText(PASSWORD), {
      target: { value: 'fixture-password' },
    });
    await waitFor(() => expect(submit).not.toBeDisabled());
    fireEvent.click(submit);
    await waitFor(() =>
      expect(handlers.onSubmit).toHaveBeenCalledWith('fixture-password')
    );
  });

  it('hides the amount in discreet mode', () => {
    renderDialog({ step: ready() });
    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );
    expect(screen.queryByText(/36\.500000/)).not.toBeInTheDocument();
  });

  it('asks for the device, not a password, on a hardware wallet', () => {
    renderDialog({
      step: ready(),
      isHardwareWallet: true,
      hwDeviceStatus: HwDeviceStatuses.CONNECTING,
    });
    expect(screen.queryByPlaceholderText(PASSWORD)).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Withdraw' })).toBeDisabled();
  });

  it('enables Withdraw on a hardware wallet once the device verified the transaction', async () => {
    const handlers = renderDialog({
      step: ready(),
      isHardwareWallet: true,
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
    });
    const submit = screen.getByRole('button', { name: 'Withdraw' });
    expect(submit).not.toBeDisabled();
    fireEvent.click(submit);
    await waitFor(() =>
      expect(handlers.onSubmit).toHaveBeenCalledWith(undefined)
    );
  });

  it('shows why nothing was sent and offers only Cancel', () => {
    const handlers = renderDialog({
      step: {
        status: 'failed',
        error: apiErrorMessages.rewardWithdrawalNotBuilt as never,
      },
    });
    expect(screen.getByRole('alert')).toHaveTextContent(
      /did not include the staking rewards in this transaction, so nothing was sent/
    );
    expect(
      screen.queryByRole('button', { name: 'Withdraw' })
    ).not.toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(handlers.onCancel).toHaveBeenCalledTimes(1);
  });

  it('shows the ledger rule with its link after a refusal', () => {
    renderDialog({
      step: {
        status: 'failed',
        error: {
          ...apiErrorMessages.withdrawalNotPossibleWithoutVote,
          values: {},
        } as never,
      },
    });
    expect(
      screen.getByRole('link', { name: 'Governance Center' })
    ).toHaveAttribute('href', '#/governance/delegate');
  });

  it('keeps the transaction signable and shows a failure before anything was sent', () => {
    renderDialog({
      step: ready({
        error: apiErrorMessages.wrongEncryptionPassphrase as never,
      }),
    });
    expect(screen.getByRole('alert')).toBeInTheDocument();
    expect(screen.getByPlaceholderText(PASSWORD)).toBeInTheDocument();
  });

  it('offers no Cancel and a disabled Withdraw while a hardware submission runs', () => {
    renderDialog({
      step: ready(),
      isHardwareWallet: true,
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
      isSubmitting: true,
    });
    expect(
      screen.queryByRole('button', { name: 'Cancel' })
    ).not.toBeInTheDocument();
    expect(screen.getByRole('button', { name: 'Withdraw' })).toBeDisabled();
  });

  it('offers no Cancel while submitting', () => {
    renderDialog({ step: { status: 'submitting' } });
    expect(
      screen.queryByRole('button', { name: 'Cancel' })
    ).not.toBeInTheDocument();
  });
});
