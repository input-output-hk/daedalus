import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { _createWalletFromServerData } from '../../api/api';
import { loadRecordedExchange } from '../../../../../tests/jest/support/walletReplay';
import { BrowserLocalStorageBridge } from '../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../features/discreet-mode';
import { AnalyticsProvider } from '../analytics';
import { noopAnalyticsTracker } from '../../analytics';
import { WalletUnits } from '../../domains/Wallet';
import type { AdaWallet } from '../../api/wallets/types';
import TopBar from './TopBar';

jest.mock('../../api/utils/request', () => ({ request: jest.fn() }));
jest.mock('../../api/utils/requestV0', () => ({ request: jest.fn() }));

const recordedWallet = (
  backend: 'upstream' | 'cip30' | 'ledger-vote',
  name: string,
  isLegacy = false
): AdaWallet => ({
  ...JSON.parse(loadRecordedExchange(backend, name).response.body),
  isLegacy,
});

const renderTopBar = (
  wallet: AdaWallet,
  { restoring = false, onDelegateHeldRewards = jest.fn() } = {}
) => {
  const activeWallet = _createWalletFromServerData(wallet);
  if (restoring) {
    activeWallet.syncState = {
      status: 'syncing',
      progress: { quantity: 40, unit: 'percentage' },
    };
  }
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <AnalyticsProvider tracker={noopAnalyticsTracker}>
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <TopBar
              activeWallet={activeWallet}
              isShelleyActivated
              onDelegateHeldRewards={onDelegateHeldRewards}
            />
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </AnalyticsProvider>
    </IntlProvider>
  );
  return { onDelegateHeldRewards, activeWallet };
};

const withReward = (wallet: AdaWallet, quantity: number): AdaWallet => ({
  ...wallet,
  balance: {
    ...wallet.balance,
    reward: { quantity, unit: WalletUnits.LOVELACE },
  },
});

const TITLE = 'Staking rewards on hold';

describe('TopBar held rewards notification', () => {
  beforeEach(() => {
    global.cip30Enabled = false;
  });
  afterEach(cleanup);

  it('shows for a wallet the ledger holds no vote delegation for', () => {
    renderTopBar(recordedWallet('ledger-vote', 'wallet-held-rewards'));
    expect(screen.getByText(TITLE)).toBeInTheDocument();
    expect(
      screen.getByText(/ADA of staking rewards cannot be withdrawn/)
    ).toBeInTheDocument();
  });

  it('hands the active wallet id to the delegate handler', () => {
    const { onDelegateHeldRewards, activeWallet } = renderTopBar(
      recordedWallet('ledger-vote', 'wallet-held-rewards')
    );
    screen.getByRole('button', { name: 'Delegate voting power' }).click();
    expect(onDelegateHeldRewards).toHaveBeenCalledWith(activeWallet.id);
  });

  it('is hidden, with a reward above zero, when the wallet does not report the ledger value', () => {
    const { activeWallet } = renderTopBar(
      withReward(
        recordedWallet('ledger-vote', 'wallet-retired-drep'),
        4886755651
      )
    );
    expect(activeWallet.reward.gt(0)).toBe(true);
    expect(activeWallet.ledgerVote).toEqual({ status: 'unknown' });
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });

  it('is hidden for a wallet delegated to Abstain', () => {
    renderTopBar(recordedWallet('ledger-vote', 'wallet-abstain'));
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });

  it('is hidden when the reward is zero', () => {
    renderTopBar(
      withReward(recordedWallet('ledger-vote', 'wallet-held-rewards'), 0)
    );
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });

  it('is hidden for a legacy wallet', () => {
    renderTopBar(recordedWallet('ledger-vote', 'wallet-held-rewards', true));
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });

  it('is hidden while the wallet is restoring', () => {
    const { activeWallet } = renderTopBar(
      recordedWallet('ledger-vote', 'wallet-held-rewards'),
      { restoring: true }
    );
    expect(activeWallet.isRestoring).toBe(true);
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });

  it('is hidden for a wallet from the cip30 build', () => {
    global.cip30Enabled = true;
    const { activeWallet } = renderTopBar(
      withReward(recordedWallet('cip30', 'wallet-retired-drep'), 4886755651)
    );
    expect(activeWallet.reward.gt(0)).toBe(true);
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });

  it('is hidden when no delegate handler is given', () => {
    const activeWallet = _createWalletFromServerData(
      recordedWallet('ledger-vote', 'wallet-held-rewards')
    );
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <AnalyticsProvider tracker={noopAnalyticsTracker}>
          <BrowserLocalStorageBridge>
            <DiscreetModeFeatureProvider>
              <TopBar activeWallet={activeWallet} isShelleyActivated />
            </DiscreetModeFeatureProvider>
          </BrowserLocalStorageBridge>
        </AnalyticsProvider>
      </IntlProvider>
    );
    expect(screen.queryByText(TITLE)).not.toBeInTheDocument();
  });
});
