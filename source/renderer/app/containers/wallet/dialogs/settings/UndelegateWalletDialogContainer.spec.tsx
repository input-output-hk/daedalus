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
import translations from '../../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../../themes/daedalus';
import { themeOverrides } from '../../../../themes/overrides';
import { formattedWalletAmount } from '../../../../utils/formatters';
import { BrowserLocalStorageBridge } from '../../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../../features/discreet-mode';
import { AnalyticsProvider } from '../../../../components/analytics';
import { noopAnalyticsTracker } from '../../../../analytics';
import { messages as errorMessages } from '../../../../api/errors';
import UndelegateWalletDialogContainer from './UndelegateWalletDialogContainer';

jest.mock('../../../../components/widgets/Dialog', () => ({
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

type Step = { status: string; [key: string]: unknown } | null;

const createFixture = ({
  decision = 'withdraw',
  isHardwareWallet = false,
  step = null,
  undelegateError,
}: {
  decision?: string;
  isHardwareWallet?: boolean;
  step?: Step;
  undelegateError?: unknown;
} = {}) => {
  const wallet = {
    id: 'wallet',
    name: 'Wallet',
    isHardwareWallet,
    delegatedStakePoolId: 'pool',
    reward: new BigNumber(5),
  };
  const state = { decision, step };
  const undelegateWallet = jest.fn();
  if (undelegateError === undefined)
    undelegateWallet.mockResolvedValue(undefined);
  else undelegateWallet.mockRejectedValue(undelegateError);
  const prepareUndelegationVote = jest.fn().mockResolvedValue(undefined);
  const submitUndelegationVote = jest.fn().mockResolvedValue(undefined);
  const cancelUndelegationVote = jest.fn();
  const selectDelegationCoins = jest.fn().mockResolvedValue({
    deposits: new BigNumber(0),
    depositsReclaimed: new BigNumber(2),
    fee: new BigNumber('0.2'),
  });
  const initiateTransaction = jest.fn();
  const request = { isExecuting: false, error: null, reset: jest.fn() };
  const calculateDelegationFee = jest.fn().mockResolvedValue({
    fee: new BigNumber('0.17'),
    deposits: new BigNumber(0),
    depositsReclaimed: new BigNumber(0),
  });
  const releaseStakePools = jest.fn();
  const acquireStakePools = jest.fn(() => releaseStakePools);
  const trigger = jest.fn();
  const close = jest.fn();
  const resetHardware = jest.fn();
  const props = {
    onExternalLinkClick: jest.fn(),
    stores: {
      wallets: {
        allWallets: [wallet],
        getWalletById: () => wallet,
        withdrawalDecisionFor: () => ({ kind: state.decision }),
        undelegationVoteStepFor: () => state.step,
        prepareUndelegationVote,
        submitUndelegationVote,
        cancelUndelegationVote,
        undelegateWalletSubmissionSuccess: false,
        _undelegateWallet: undelegateWallet,
      },
      staking: {
        calculateDelegationFee,
        quitStakePoolRequest: request,
        isDelegationTransactionPending: false,
        getStakePoolById: () => ({ name: 'Pool', ticker: 'POOL' }),
        acquireStakePools,
      },
      networkStatus: { futureEpoch: {} },
      profile: { currentLocale: 'en-US' },
      uiDialogs: { dataForActiveDialog: { walletId: 'wallet' } },
      hardwareWallets: {
        sendMoneyRequest: request,
        selectCoinsRequest: request,
        checkIsTrezorByWalletId: () => false,
        hwDeviceStatus: 'ready',
        selectDelegationCoins,
        initiateTransaction,
        _resetTransaction: resetHardware,
      },
    },
    actions: {
      wallets: {
        undelegateWallet: { trigger },
        setUndelegateWalletSubmissionSuccess: { trigger: jest.fn() },
      },
      dialogs: { closeActiveDialog: { trigger: close } },
    },
  };
  let tick = 0;
  const element = () => (
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
              <UndelegateWalletDialogContainer
                {...({ ...props, tick } as unknown as React.ComponentProps<
                  typeof UndelegateWalletDialogContainer
                >)}
              />
            </DiscreetModeFeatureProvider>
          </BrowserLocalStorageBridge>
        </AnalyticsProvider>
      </IntlProvider>
    </ThemeProvider>
  );
  const view = render(element());
  // Changes what the mocked stores answer and renders again.
  const update = (next: { decision?: string; step?: Step }) => {
    if (next.decision != null) state.decision = next.decision;
    if ('step' in next) state.step = next.step ?? null;
    tick += 1;
    view.rerender(element());
  };
  return {
    ...view,
    update,
    trigger,
    close,
    resetHardware,
    acquireStakePools,
    releaseStakePools,
    calculateDelegationFee,
    undelegateWallet,
    prepareUndelegationVote,
    submitUndelegationVote,
    cancelUndelegationVote,
    selectDelegationCoins,
    initiateTransaction,
  };
};

describe('upstream undelegation confirmation', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });
  afterEach(cleanup);

  it('asks for the stake pool list while it is open and withdraws the ask when it closes', () => {
    const fixture = createFixture();
    expect(fixture.acquireStakePools).toHaveBeenCalledTimes(1);
    expect(fixture.releaseStakePools).not.toHaveBeenCalled();
    fixture.unmount();
    expect(fixture.releaseStakePools).toHaveBeenCalledTimes(1);
  });

  it('shows the fixed upstream deposit refund and fee before any submission', async () => {
    const fixture = createFixture();
    await waitFor(() =>
      expect(
        screen.getByText(formattedWalletAmount(new BigNumber(2), false))
      ).toBeInTheDocument()
    );
    expect(
      screen.getByText(formattedWalletAmount(new BigNumber('0.17'), false))
    ).toBeInTheDocument();
    expect(fixture.trigger).not.toHaveBeenCalled();
    expect(fixture.calculateDelegationFee).toHaveBeenCalledWith({
      walletId: 'wallet',
    });
  });

  it('cancels software confirmation without resetting unrelated hardware work', async () => {
    const fixture = createFixture();
    await waitFor(() =>
      expect(fixture.calculateDelegationFee).toHaveBeenCalled()
    );
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(fixture.close).toHaveBeenCalledTimes(1);
    expect(fixture.resetHardware).not.toHaveBeenCalled();
    expect(fixture.trigger).not.toHaveBeenCalled();
  });

  it('submits the undelegation on mount for a cip30 wallet that needs no vote delegation', async () => {
    global.cip30Enabled = true;
    const fixture = createFixture();
    await waitFor(() =>
      expect(fixture.undelegateWallet).toHaveBeenCalledWith(
        expect.objectContaining({ walletId: 'wallet', passphrase: '' })
      )
    );
    expect(fixture.prepareUndelegationVote).not.toHaveBeenCalled();
    expect(fixture.calculateDelegationFee).not.toHaveBeenCalled();
  });

  it('shows a construct refusal that carries a message in an error dialog instead of closing', async () => {
    global.cip30Enabled = true;
    const fixture = createFixture({
      undelegateError: {
        ...errorMessages.withdrawalNotPossibleWithoutVote,
        values: {},
      },
    });
    expect(
      await screen.findByText(
        /This transaction withdraws this wallet's staking rewards\./
      )
    ).toBeInTheDocument();
    expect(
      screen.getByRole('link', { name: 'Governance Center' })
    ).toHaveAttribute('href', '#/governance/delegate');
    expect(fixture.close).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Close' }));
    expect(fixture.close).toHaveBeenCalledTimes(1);
  });

  it('closes the dialog when the failure is an approval outcome with no message', async () => {
    global.cip30Enabled = true;
    const fixture = createFixture({ undelegateError: new Error('cancelled') });
    await waitFor(() => expect(fixture.close).toHaveBeenCalledTimes(1));
    expect(screen.queryByText(/staking rewards\./)).not.toBeInTheDocument();
  });

  it("runs today's flow when the decision omits the withdrawal at a zero reward", async () => {
    const fixture = createFixture({ decision: 'omit' });
    await waitFor(() =>
      expect(fixture.calculateDelegationFee).toHaveBeenCalled()
    );
    expect(fixture.prepareUndelegationVote).not.toHaveBeenCalled();
  });
});

describe('undelegation with a vote delegation first', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });
  afterEach(cleanup);

  it('prepares the vote step when none exists and shows its fee for a software wallet', async () => {
    const fixture = createFixture({ decision: 'delegate_vote_first' });
    expect(fixture.prepareUndelegationVote).toHaveBeenCalledWith('wallet');
    expect(screen.getByText('Calculating fees')).toBeInTheDocument();
    fixture.update({
      step: { status: 'ready', fee: new BigNumber('0.18') },
    });
    expect(
      screen.getByText('Step 1 of 2: delegate voting power to Abstain')
    ).toBeInTheDocument();
    expect(
      screen.getByText(formattedWalletAmount(new BigNumber('0.18'), false))
    ).toBeInTheDocument();
    expect(fixture.calculateDelegationFee).not.toHaveBeenCalled();
  });

  it('resumes a wait in progress without preparing the step again', () => {
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      step: { status: 'awaiting_ledger', transactionId: 'tx' },
    });
    expect(fixture.prepareUndelegationVote).not.toHaveBeenCalled();
  });

  it('submits the vote step with the spending password', async () => {
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      step: { status: 'ready', fee: new BigNumber('0.18') },
    });
    fireEvent.change(
      screen.getByPlaceholderText('Type your spending password here'),
      { target: { value: 'secret' } }
    );
    await waitFor(() =>
      expect(
        screen.getByRole('button', { name: 'Delegate to Abstain' })
      ).not.toBeDisabled()
    );
    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    );
    await waitFor(() =>
      expect(fixture.submitUndelegationVote).toHaveBeenCalledWith(
        'wallet',
        'secret'
      )
    );
  });

  it('waits for the ledger and then starts the undelegation', async () => {
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      step: { status: 'awaiting_ledger', transactionId: 'tx' },
    });
    expect(
      screen.getByText(/Waiting for the delegation to Abstain/)
    ).toBeInTheDocument();
    expect(screen.queryByRole('button', { name: 'Cancel' })).toBeNull();
    expect(fixture.calculateDelegationFee).not.toHaveBeenCalled();
    fixture.update({ decision: 'withdraw', step: null });
    await waitFor(() =>
      expect(fixture.calculateDelegationFee).toHaveBeenCalledWith({
        walletId: 'wallet',
      })
    );
    expect(await screen.findByText('Fees')).toBeInTheDocument();
    expect(
      screen.queryByText(/Waiting for the delegation to Abstain/)
    ).toBeNull();
  });

  it('closes the dialog without cancelling the wait', () => {
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      step: { status: 'awaiting_ledger', transactionId: 'tx' },
    });
    fireEvent.click(screen.getByRole('button', { name: 'Close' }));
    expect(fixture.close).toHaveBeenCalledTimes(1);
    expect(fixture.cancelUndelegationVote).not.toHaveBeenCalled();
  });

  it('cancels the vote step and the hardware transaction', () => {
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      isHardwareWallet: true,
    });
    fixture.update({ step: { status: 'ready', fee: new BigNumber('0.18') } });
    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(fixture.cancelUndelegationVote).toHaveBeenCalledWith('wallet');
    expect(fixture.resetHardware).toHaveBeenCalledWith({
      cancelDeviceAction: true,
    });
    expect(fixture.close).toHaveBeenCalledTimes(1);
  });

  it('selects the quit coins for a hardware wallet only after the ledger reads delegated', async () => {
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      isHardwareWallet: true,
    });
    fixture.update({
      step: { status: 'awaiting_ledger', transactionId: 'tx' },
    });
    expect(fixture.selectDelegationCoins).not.toHaveBeenCalled();
    fixture.update({ decision: 'withdraw', step: null });
    await waitFor(() =>
      expect(fixture.selectDelegationCoins).toHaveBeenCalledWith(
        expect.objectContaining({ walletId: 'wallet' })
      )
    );
    await waitFor(() =>
      expect(fixture.initiateTransaction).toHaveBeenCalledWith({
        walletId: 'wallet',
      })
    );
  });

  it('shows the hardware device status in the vote step', () => {
    createFixture({
      decision: 'delegate_vote_first',
      isHardwareWallet: true,
      step: { status: 'ready', fee: new BigNumber('0.18') },
    });
    expect(
      screen.queryByPlaceholderText('Type your spending password here')
    ).toBeNull();
    expect(
      screen.getByRole('button', { name: 'Delegate to Abstain' })
    ).toBeDisabled();
  });

  it('shows the cip30 vote step without a fee and submits nothing yet', async () => {
    global.cip30Enabled = true;
    const fixture = createFixture({
      decision: 'delegate_vote_first',
      step: { status: 'ready', fee: null },
    });
    expect(
      screen.getByText('Step 1 of 2: delegate voting power to Abstain')
    ).toBeInTheDocument();
    expect(screen.queryByText('Fees')).toBeNull();
    expect(fixture.trigger).not.toHaveBeenCalled();
  });
});
