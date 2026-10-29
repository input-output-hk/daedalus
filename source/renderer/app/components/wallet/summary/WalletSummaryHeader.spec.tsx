import React from 'react';
import BigNumber from 'bignumber.js';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { messages as apiErrorMessages } from '../../../api/errors';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import WalletSummaryHeader from './WalletSummaryHeader';
import type { RewardsAction } from './WalletSummaryHeader';

const renderHeader = ({
  rewardsAction,
  isLegacy = false,
  isRestoring = false,
}: {
  rewardsAction?: RewardsAction | null;
  isLegacy?: boolean;
  isRestoring?: boolean;
}) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <WalletSummaryHeader
              wallet={
                {
                  name: 'Wallet',
                  amount: new BigNumber(100),
                  isLegacy,
                  isRestoring,
                } as never
              }
              reward={
                {
                  total: new BigNumber(40),
                  unspent: new BigNumber(40),
                } as never
              }
              numberOfRecentTransactions={0}
              numberOfPendingTransactions={0}
              isLoadingTransactions
              rewardsAction={rewardsAction}
            />
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </IntlProvider>
    </ThemeProvider>
  );

const action = (overrides: Partial<RewardsAction> = {}): RewardsAction => ({
  disabled: false,
  isSubmitting: false,
  error: null,
  onWithdraw: jest.fn(),
  onDelegate: jest.fn(),
  ...overrides,
});

describe('WalletSummaryHeader Withdraw rewards', () => {
  afterEach(cleanup);

  it('draws the button beside the rewards and starts the withdrawal', () => {
    const rewardsAction = action();
    renderHeader({ rewardsAction });
    fireEvent.click(screen.getByRole('button', { name: 'Withdraw rewards' }));
    expect(rewardsAction.onWithdraw).toHaveBeenCalledTimes(1);
  });

  it.each([
    ['no action', null],
    ['an absent action', undefined],
  ])('draws no button for %s', (_name, rewardsAction) => {
    renderHeader({ rewardsAction });
    expect(
      screen.queryByRole('button', { name: 'Withdraw rewards' })
    ).not.toBeInTheDocument();
  });

  it('draws no button for a legacy wallet', () => {
    renderHeader({ rewardsAction: action(), isLegacy: true });
    expect(
      screen.queryByRole('button', { name: 'Withdraw rewards' })
    ).not.toBeInTheDocument();
  });

  it('disables the button while the wallet restores', () => {
    renderHeader({ rewardsAction: action(), isRestoring: true });
    expect(
      screen.getByRole('button', { name: 'Withdraw rewards' })
    ).toBeDisabled();
  });

  it('disables the button with the reason and the delegation link while the voting power is not delegated', () => {
    const rewardsAction = action({
      disabled: true,
      reason: 'ledger_not_delegated',
    });
    renderHeader({ rewardsAction });
    expect(
      screen.getByRole('button', { name: 'Withdraw rewards' })
    ).toBeDisabled();
    expect(
      screen.getByText(
        /Staking rewards cannot be withdrawn while this wallet's voting power is not delegated\./
      )
    ).toBeInTheDocument();
    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate voting power' })
    );
    expect(rewardsAction.onDelegate).toHaveBeenCalledTimes(1);
  });

  it('shows a message under the button', () => {
    renderHeader({
      rewardsAction: action({
        error: apiErrorMessages.rewardWithdrawalNotBuilt as never,
      }),
    });
    expect(screen.getByRole('alert')).toHaveTextContent(
      /did not include the staking rewards/
    );
  });
});
