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
import { formattedWalletAmount } from '../../../utils/formatters';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import type { UndelegationVoteStep } from '../../../stores/WalletsStore';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import {
  DiscreetModeFeatureProvider,
  DiscreetModeToggle,
} from '../../../features/discreet-mode';
import { AnalyticsProvider } from '../../analytics';
import { noopAnalyticsTracker } from '../../../analytics';
import UndelegateWalletVoteFirstDialog from './UndelegateWalletVoteFirstDialog';

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
}: {
  step: UndelegationVoteStep | null;
  isHardwareWallet?: boolean;
  hwDeviceStatus?: string;
}) => {
  const handlers = {
    onSubmit: jest.fn(),
    onRetry: jest.fn(),
    onCancel: jest.fn(),
    onClose: jest.fn(),
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
                <UndelegateWalletVoteFirstDialog
                  walletName="Wallet"
                  heldReward={new BigNumber(5)}
                  isHardwareWallet={isHardwareWallet}
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

const STEP_ONE = 'Step 1 of 2: delegate voting power to Abstain';
const PASSWORD = 'Type your spending password here';

describe('UndelegateWalletVoteFirstDialog', () => {
  afterEach(cleanup);

  it('shows the explanation, a fee placeholder and only Cancel while estimating', () => {
    const handlers = renderDialog({ step: { status: 'estimating' } });
    expect(
      screen.getByText(/Undelegation therefore takes two transactions/)
    ).toBeInTheDocument();
    expect(
      screen.getByText(/withdraws its 5\.000000 ADA of staking rewards/)
    ).toBeInTheDocument();
    expect(screen.getByText('Calculating fees')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    ).toBeDisabled();
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(handlers.onCancel).toHaveBeenCalledTimes(1);
  });

  it('hides the held reward in discreet mode', () => {
    renderDialog({ step: { status: 'estimating' } });
    fireEvent.click(
      screen.getByRole('button', { name: /discreetModeToggle/i })
    );
    expect(screen.queryByText(/5\.000000/)).not.toBeInTheDocument();
    expect(
      screen.getByText(/withdraws its \*\*\* ADA of staking rewards/)
    ).toBeInTheDocument();
  });

  it('reads a wallet without a step as estimating', () => {
    renderDialog({ step: null });
    expect(screen.getByText('Calculating fees')).toBeInTheDocument();
  });

  it('shows the step heading, the fee and the password field when ready', () => {
    renderDialog({ step: { status: 'ready', fee: new BigNumber('0.18') } });
    expect(screen.getByText(STEP_ONE)).toBeInTheDocument();
    expect(
      screen.getByText(formattedWalletAmount(new BigNumber('0.18'), false))
    ).toBeInTheDocument();
    expect(screen.getByPlaceholderText(PASSWORD)).toBeInTheDocument();
  });

  it('submits the typed password from Delegate to Abstain', async () => {
    const handlers = renderDialog({
      step: { status: 'ready', fee: new BigNumber('0.18') },
    });
    expect(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    ).toBeDisabled();
    fireEvent.change(screen.getByPlaceholderText(PASSWORD), {
      target: { value: 'secret' },
    });
    await waitFor(() =>
      expect(
        screen.getByRole('button', { name: 'Delegate to Abstain' })
      ).not.toBeDisabled()
    );
    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    );
    await waitFor(() =>
      expect(handlers.onSubmit).toHaveBeenCalledWith('secret')
    );
  });

  it('shows the device status instead of a password field for a hardware wallet', () => {
    renderDialog({
      step: { status: 'ready', fee: new BigNumber('0.18') },
      isHardwareWallet: true,
    });
    expect(screen.queryByPlaceholderText(PASSWORD)).toBeNull();
    expect(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    ).toBeDisabled();
  });

  it('enables the hardware wallet button once the device verified the transaction', async () => {
    const handlers = renderDialog({
      step: { status: 'ready', fee: new BigNumber('0.18') },
      isHardwareWallet: true,
      hwDeviceStatus: HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED,
    });
    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    );
    await waitFor(() =>
      expect(handlers.onSubmit).toHaveBeenCalledWith(undefined)
    );
  });

  it('shows no fee row for a cip30 wallet whose approval dialog shows it', () => {
    renderDialog({ step: { status: 'ready', fee: null } });
    expect(screen.queryByText('Fees')).toBeNull();
    expect(screen.queryByText('Calculating fees')).toBeNull();
  });

  it('offers no action but a disabled button while submitting', () => {
    renderDialog({ step: { status: 'submitting' } });
    expect(screen.queryByRole('button', { name: 'Cancel' })).toBeNull();
    expect(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    ).toBeDisabled();
  });

  it('shows the waiting message and only Close while awaiting the ledger', () => {
    const handlers = renderDialog({
      step: { status: 'awaiting_ledger', transactionId: 'tx' },
    });
    expect(
      screen.getByText(
        'Waiting for the delegation to Abstain to be confirmed. The undelegation follows once it is.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Cancel' })).toBeNull();
    expect(
      screen.queryByRole('button', { name: 'Delegate to Abstain' })
    ).toBeNull();
    fireEvent.click(screen.getByRole('button', { name: 'Close' }));
    expect(handlers.onClose).toHaveBeenCalledTimes(1);
    expect(handlers.onCancel).not.toHaveBeenCalled();
  });

  it('says the delegation was not confirmed and offers Cancel and Retry', () => {
    const handlers = renderDialog({
      step: { status: 'failed', errorCode: 'vote_not_confirmed' },
    });
    expect(
      screen.getByText(
        'The delegation to Abstain was not confirmed. No undelegation was sent.'
      )
    ).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'Retry' }));
    expect(handlers.onRetry).toHaveBeenCalledTimes(1);
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(handlers.onCancel).toHaveBeenCalledTimes(1);
  });

  it('shows the governance form message for an error code', () => {
    renderDialog({ step: { status: 'failed', errorCode: 'same_vote' } });
    expect(
      screen.getByText(/This wallet already delegates to this choice/)
    ).toBeInTheDocument();
  });

  it('shows the incorrect password message for a wrong passphrase', () => {
    renderDialog({
      step: { status: 'failed', errorCode: 'wrong_encryption_passphrase' },
    });
    expect(screen.getByText('Incorrect wallet password.')).toBeInTheDocument();
  });
});
