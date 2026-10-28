import React from 'react';
import BigNumber from 'bignumber.js';
import { bech32 } from 'bech32';
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
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';

const buildWallet = (
  id: string,
  name: string,
  isLegacy: boolean,
  vote: Partial<Pick<Wallet, 'votingTarget' | 'ledgerVote'>> = {}
): Wallet =>
  new Wallet({
    ...vote,
    id,
    name,
    isLegacy,
    addressPoolGap: 20,
    singleAddressMode: !isLegacy,
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
  onCancel: () => void = () => undefined,
  submitTransaction: React.ComponentProps<
    typeof VotingPowerDelegation
  >['submitTransaction'] = async () => ({ success: true }),
  onFetchDRep?: jest.Mock
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
              submitTransaction={submitTransaction}
              onFetchDRep={onFetchDRep}
              onBrowseDRepsClick={() => undefined}
              onCancel={onCancel}
              onExternalLinkClick={() => undefined}
              onSuccess={() => undefined}
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

  describe('without the Directory', () => {
    const typeId = (value: string) =>
      fireEvent.change(screen.getByPlaceholderText('Paste a DRep ID'), {
        target: { value },
      });

    it('offers Abstain, No Confidence, a DRep ID field and Browse DReps before anything is chosen', () => {
      renderForm([SHELLEY], 'shelley-1');

      expect(
        screen.getByText(/Choose Abstain, No Confidence or a DRep\./)
      ).toBeInTheDocument();
      expect(screen.getByText('Abstain')).toBeInTheDocument();
      expect(screen.getByText('No Confidence')).toBeInTheDocument();
      expect(screen.getAllByRole('button', { name: 'Select' })).toHaveLength(2);
      expect(
        screen.getByPlaceholderText('Paste a DRep ID')
      ).toBeInTheDocument();
      expect(
        screen.getByRole('button', { name: 'Browse DReps' })
      ).toBeInTheDocument();
    });

    it('makes Abstain the choice, and Change returns to the choices', () => {
      renderForm([SHELLEY], 'shelley-1');

      fireEvent.click(screen.getAllByRole('button', { name: 'Select' })[0]);
      expect(screen.getByRole('button', { name: 'Submit' })).toBeEnabled();
      expect(
        screen.queryByText(/Choose Abstain, No Confidence or a DRep\./)
      ).not.toBeInTheDocument();

      fireEvent.click(screen.getByRole('button', { name: 'Change' }));
      expect(
        screen.getByText(/Choose Abstain, No Confidence or a DRep\./)
      ).toBeInTheDocument();
    });

    it('makes a pasted DRep ID the choice in its CIP-129 form', () => {
      renderForm([SHELLEY], 'shelley-1');
      // The earlier encoding: the 28 credential bytes under the `drep` prefix.
      const legacy = bech32.encode(
        'drep',
        bech32.toWords(
          Buffer.from(
            normalizeDRepIdentity(
              'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras'
            )!.credentialHex!,
            'hex'
          )
        )
      );

      typeId(`  ${legacy}  `);
      fireEvent.click(screen.getByRole('button', { name: 'Use this DRep ID' }));

      expect(
        screen.getByLabelText(
          'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras'
        )
      ).toBeInTheDocument();
      expect(screen.getByRole('button', { name: 'Submit' })).toBeEnabled();
    });

    it('looks nothing up for Abstain or No Confidence', () => {
      const onFetchDRep = jest.fn().mockResolvedValue(null);
      renderForm(
        [SHELLEY],
        'shelley-1',
        undefined,
        undefined,
        undefined,
        onFetchDRep
      );

      fireEvent.click(screen.getAllByRole('button', { name: 'Select' })[0]);
      fireEvent.click(screen.getByRole('button', { name: 'Change' }));
      fireEvent.click(screen.getAllByRole('button', { name: 'Select' })[1]);

      expect(onFetchDRep).not.toHaveBeenCalled();
    });

    it('refuses a pasted value that is not a DRep ID', () => {
      renderForm([SHELLEY], 'shelley-1');

      typeId('drep1notanid');
      fireEvent.click(screen.getByRole('button', { name: 'Use this DRep ID' }));

      expect(
        screen.getByText('This is not a valid DRep ID.')
      ).toBeInTheDocument();
      expect(
        screen.queryByRole('button', { name: 'Submit' })
      ).not.toBeInTheDocument();
    });

    describe('with the cip30 submission path', () => {
      const originalCapability = global.cip30Enabled;
      beforeEach(() => {
        global.cip30Enabled = true;
      });
      afterEach(() => {
        global.cip30Enabled = originalCapability;
      });

      it('submits a choice while no lookup has answered', () => {
        const submit = jest.fn(async () => ({ success: true as const }));
        renderForm([SHELLEY], 'shelley-1', undefined, undefined, submit);

        fireEvent.click(screen.getAllByRole('button', { name: 'Select' })[1]);
        fireEvent.click(screen.getByRole('button', { name: 'Submit' }));

        expect(submit).toHaveBeenCalledWith({
          chosenOption: 'no_confidence',
          wallet: SHELLEY,
        });
      });
    });
  });

  describe('when the DRep of the wallet retired', () => {
    const DREP_ID =
      'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
    const drep = normalizeDRepIdentity(DREP_ID)!;
    const voting = { kind: 'drep' as const, drep, source: 'ledger' as const };
    const submit = () => screen.getByRole('button', { name: 'Submit' });

    it('lets the wallet delegate back to the DRep once the ledger holds no delegation', () => {
      const ended = buildWallet('shelley-1', 'Shelley wallet', false, {
        votingTarget: voting,
        ledgerVote: { status: 'not_delegated' },
      });
      renderForm([ended], 'shelley-1', DREP_ID);

      expect(submit()).toBeEnabled();
    });

    it('refuses to delegate to the DRep the ledger already delegates to', () => {
      const delegated = buildWallet('shelley-1', 'Shelley wallet', false, {
        votingTarget: voting,
        ledgerVote: { status: 'delegated', voting },
      });
      renderForm([delegated], 'shelley-1', DREP_ID);

      expect(submit()).toBeDisabled();
    });
  });

  describe('when the wallet refuses the delegation', () => {
    const originalCapability = global.cip30Enabled;
    beforeEach(() => {
      global.cip30Enabled = true;
    });
    afterEach(() => {
      global.cip30Enabled = originalCapability;
    });
    const DREP_ID =
      'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';
    const refuse = async (errorCode: string) => {
      renderForm([SHELLEY], SHELLEY.id, DREP_ID, undefined, async () => ({
        success: false,
        errorCode,
      }));
      fireEvent.click(screen.getByRole('button', { name: 'Submit' }));
    };

    it('names another DRep, Abstain and No Confidence as the way out of same_vote', async () => {
      await refuse('same_vote');
      expect(
        await screen.findByText(
          translations['voting.governance.transactionError.sameVote']
        )
      ).toBeInTheDocument();
    });

    it('links the wallet transactions when a pending transaction holds the funds', async () => {
      await refuse('pending_transaction');
      expect(
        await screen.findByText(/held by a pending transaction/)
      ).toBeInTheDocument();
      expect(
        screen.getByRole('link', { name: "wallet's transaction list" })
      ).toHaveAttribute('href', '#/wallets/shelley-1/transactions');
    });

    it('says the DRep is not registered for not_found', async () => {
      await refuse('not_found');
      expect(
        await screen.findByText(translations['api.errors.drepNotRegistered'])
      ).toBeInTheDocument();
    });
  });
});
