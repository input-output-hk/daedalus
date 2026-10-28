import React from 'react';
import { IntlProvider } from 'react-intl';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { SimpleDefaults } from 'react-polymorph/lib/themes/simple';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import { daedalusTheme } from '../../../themes/daedalus';
import { themeOverrides } from '../../../themes/overrides';
import { normalizeDRepIdentity } from '../../../utils/governance/normalizeDRepIdentity';
import GovernanceWallets from './GovernanceWallets';
import type { WalletDelegationSummary } from './GovernanceWallets';

const DREP_ID = 'drep1yg7svuv02gh9j2q574jv06l4xnzwyp63effljze28qe993caj8ras';
const drep = normalizeDRepIdentity(DREP_ID)!;
const entry = {
  drepId: DREP_ID,
  votingPower: null,
  status: 'active' as const,
  drepActivity: 30,
  anchor: null,
  verifiedName: 'Ada Voter',
  doNotList: false,
};

const renderWallets = (
  wallets: Array<WalletDelegationSummary>,
  handlers: Partial<{
    onChangeDelegation: (id: string) => void;
    onViewDetails: (drepId: string, walletId: string) => void;
  }> = {}
) =>
  render(
    <ThemeProvider
      theme={daedalusTheme}
      skins={SimpleSkins}
      variables={SimpleDefaults}
      themeOverrides={themeOverrides}
    >
      <IntlProvider locale="en-US" messages={translations}>
        <GovernanceWallets
          wallets={wallets}
          favoriteDRepIds={new Set()}
          onToggleFavorite={jest.fn()}
          onChangeDelegation={handlers.onChangeDelegation ?? jest.fn()}
          onViewDetails={handlers.onViewDetails ?? jest.fn()}
          onExternalLinkClick={jest.fn()}
        />
      </IntlProvider>
    </ThemeProvider>
  );

const row = (
  state: WalletDelegationSummary['state'],
  lookup: WalletDelegationSummary['lookup'] = { status: 'idle' }
): WalletDelegationSummary => ({
  walletId: 'wallet-1',
  walletName: 'Savings',
  state,
  lookup,
});

describe('GovernanceWallets', () => {
  afterEach(cleanup);

  it('starts delegation with the wallet selected in the Governance Center', () => {
    const onChangeDelegation = jest.fn();
    renderWallets([{ ...row({ kind: 'none' }), walletId: 'wallet-2' }], {
      onChangeDelegation,
    });

    fireEvent.click(screen.getByRole('button', { name: 'Delegate' }));

    expect(onChangeDelegation).toHaveBeenCalledWith('wallet-2');
  });

  it('shows a delegation to a DRep with its name, badge and details', () => {
    const onViewDetails = jest.fn();
    renderWallets(
      [
        row(
          {
            kind: 'current',
            voting: { kind: 'drep', drep, source: 'ledger' },
            source: 'ledger',
          },
          { status: 'found', entry }
        ),
      ],
      { onViewDetails }
    );

    expect(screen.getByText('Ada Voter')).toBeInTheDocument();
    expect(screen.getByText('Active')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Redelegate' })
    ).toBeInTheDocument();
    fireEvent.click(screen.getByRole('button', { name: 'View details' }));
    expect(onViewDetails).toHaveBeenCalledWith(DREP_ID, 'wallet-1');
  });

  it('says a DRep is loading only while its lookup is outstanding', () => {
    const current = {
      kind: 'current',
      voting: { kind: 'drep', drep, source: 'ledger' },
      source: 'ledger',
    } as const;
    renderWallets([row(current, { status: 'pending' })]);
    expect(screen.getByText('Loading DRep…')).toBeInTheDocument();
    cleanup();
    renderWallets([row(current, { status: 'failed' })]);
    expect(screen.queryByText('Loading DRep…')).not.toBeInTheDocument();
    expect(screen.getByText('DRep data unavailable')).toBeInTheDocument();
    expect(screen.getByLabelText(DREP_ID)).toBeInTheDocument();
  });

  it('shows an ended delegation as not delegated, with the DRep it last had', () => {
    renderWallets([
      row(
        { kind: 'ended', drep, registeredAgain: false },
        { status: 'not_found' }
      ),
    ]);

    expect(screen.getByText('Undelegated')).toBeInTheDocument();
    expect(screen.getByText('Last delegated to')).toBeInTheDocument();
    expect(screen.getByLabelText(DREP_ID)).toBeInTheDocument();
    expect(screen.getByText('DRep retired')).toBeInTheDocument();
    expect(screen.queryByText('Loading DRep…')).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Delegate' })
    ).toBeInTheDocument();
  });

  it('says a DRep retired and registered again did not get its delegation back', () => {
    renderWallets([
      row(
        { kind: 'ended', drep, registeredAgain: true },
        { status: 'found', entry }
      ),
    ]);

    expect(
      screen.getByText('DRep retired and registered again')
    ).toBeInTheDocument();
  });

  it('says the wallet registry lacks a DRep where the ledger delegation is unknown', () => {
    renderWallets([
      row({ kind: 'not_in_registry', drep }, { status: 'not_found' }),
    ]);

    expect(screen.getByText('Not in the DRep registry')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Redelegate' })
    ).toBeInTheDocument();
  });

  it('lists the notice for the wallets whose delegation ended above the table', () => {
    const onChangeDelegation = jest.fn();
    render(
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        variables={SimpleDefaults}
        themeOverrides={themeOverrides}
      >
        <IntlProvider locale="en-US" messages={translations}>
          <GovernanceWallets
            wallets={[row({ kind: 'ended', drep, registeredAgain: false })]}
            endedDelegations={[
              {
                walletId: 'wallet-1',
                walletName: 'Savings',
                state: { kind: 'ended', drep, registeredAgain: false },
                heldReward: { gt: () => false } as any,
              },
            ]}
            favoriteDRepIds={new Set()}
            onToggleFavorite={jest.fn()}
            onChangeDelegation={onChangeDelegation}
            onViewDetails={jest.fn()}
            onExternalLinkClick={jest.fn()}
          />
        </IntlProvider>
      </ThemeProvider>
    );

    expect(
      screen.getByText('Voting power no longer delegated')
    ).toBeInTheDocument();
    fireEvent.click(
      screen.getByRole('button', { name: 'Delegate voting power' })
    );
    expect(onChangeDelegation).toHaveBeenCalledWith('wallet-1');
  });
});
