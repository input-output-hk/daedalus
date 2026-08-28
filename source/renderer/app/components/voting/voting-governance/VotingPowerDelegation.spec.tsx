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
import VotingPowerDelegation from './VotingPowerDelegation';
import { BrowserLocalStorageBridge } from '../../../features/local-storage';
import { DiscreetModeFeatureProvider } from '../../../features/discreet-mode';
import Wallet, { WalletSyncStateStatuses } from '../../../domains/Wallet';

const buildWallet = (id: string, name: string, isLegacy: boolean): Wallet =>
  new Wallet({
    id,
    name,
    isLegacy,
    addressPoolGap: 20,
    amount: new BigNumber(100),
    availableAmount: new BigNumber(100),
    reward: new BigNumber(0),
    assets: { available: [], total: [] },
    hasPassword: true,
    passwordUpdateDate: new Date(),
    syncState: { status: WalletSyncStateStatuses.READY },
    isHardwareWallet: false,
    discovery: 'random',
    delegatedStakePoolId: null,
  });

const SHELLEY = buildWallet('shelley-1', 'Shelley wallet', false);
const BYRON = buildWallet('byron-1', 'Byron legacy wallet', true);

const renderForm = (
  wallets: Wallet[],
  selectedWalletId?: string,
  selectedDRepId?: string,
  onCancel: () => void = () => undefined
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        {/* WalletsDropdown reads the discreet-mode feature to blur balances,
            and that feature reads local storage. */}
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <VotingPowerDelegation
              drepIndex={new Map()}
              initialFormState={
                selectedWalletId
                  ? {
                      selectedWalletId,
                      ...(selectedDRepId ? { selectedDRepId } : {}),
                    }
                  : undefined
              }
              getStakePoolById={() => null}
              initiateTransaction={async () => ({
                success: true,
                fees: new BigNumber(0),
              })}
              onBrowseDRepsClick={() => undefined}
              onCancel={onCancel}
              onExternalLinkClick={() => undefined}
              renderConfirmationDialog={() => <div />}
              stakePools={[]}
              wallets={wallets}
            />
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </IntlProvider>
    </ThemeProvider>
  );

describe('VotingPowerDelegation', () => {
  afterEach(cleanup);

  it('offers Shelley wallets and never Byron ones', () => {
    renderForm([SHELLEY, BYRON]);

    // A Byron wallet has no stake credential, so no delegation certificate
    // can be built for it. Listing it offers an action that always fails.
    expect(screen.getByText('Shelley wallet')).toBeInTheDocument();
    expect(screen.queryByText('Byron legacy wallet')).not.toBeInTheDocument();
  });

  it('ignores a Byron wallet preselected by the directory round trip', () => {
    // The wallet id travels through GovernanceStore.delegationNavState, which
    // carries an id and never re-checks what the wallet is. Selecting one the
    // dropdown itself would refuse to offer has to be impossible here too, or
    // the filter above is a cosmetic one.
    renderForm([SHELLEY, BYRON], 'byron-1');

    expect(screen.queryByText('Byron legacy wallet')).not.toBeInTheDocument();
    // Nothing is selected, so the form stops at the wallet select rather than
    // going on to offer a delegation for a wallet that cannot hold one.
    expect(screen.queryByText('Delegate to')).not.toBeInTheDocument();
  });

  it('keeps a preselected Shelley wallet', () => {
    renderForm([SHELLEY, BYRON], 'shelley-1');

    expect(screen.getAllByText('Shelley wallet').length).toBeGreaterThan(0);
  });

  it('names Abstain and says what it does, with nothing to copy', () => {
    // The directory hands this over in the same field as a DRep id, but there
    // is no credential behind it: an identifier display would offer a copy
    // button for a word, and a status badge for a registration that does not
    // exist.
    renderForm([SHELLEY], 'shelley-1', 'abstain');

    expect(screen.getAllByText('Abstain').length).toBeGreaterThan(0);
    expect(
      screen.getByText(
        'Your stake is recorded on chain as not participating in governance.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('abstain')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /copy/i })
    ).not.toBeInTheDocument();
  });

  it('names No Confidence and says what it does', () => {
    renderForm([SHELLEY], 'shelley-1', 'no_confidence');

    expect(screen.getAllByText('No Confidence').length).toBeGreaterThan(0);
    expect(
      screen.getByText(
        'Your stake counts as Yes on every motion of no confidence, and as No on every other governance action.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText('no_confidence')).not.toBeInTheDocument();
  });

  it('offers a way off the screen before anything has been chosen', () => {
    // Change goes on to the directory rather than back, so without this the
    // screen has no exit that does not look like continuing.
    const onCancel = jest.fn();
    renderForm([SHELLEY], 'shelley-1', undefined, onCancel);

    fireEvent.click(screen.getByRole('button', { name: 'Cancel' }));
    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  it('still shows a DRep id as an identifier', () => {
    const drepId = 'drep1ytnglv2y7s8dxpmylw35egsum63yqzcm0upvkf7qffg4hhqnhj0yh';
    renderForm([SHELLEY], 'shelley-1', drepId);

    // The sentinel branch must not swallow the case it was carved out of.
    expect(
      screen.queryByText(
        'Your stake is recorded on chain as not participating in governance.'
      )
    ).not.toBeInTheDocument();
  });
});
