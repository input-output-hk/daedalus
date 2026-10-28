import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import GovernanceShell from './_utils/GovernanceShell';
import { ROUTES } from '../../../source/renderer/app/routes-config';
import GovernanceWallets from '../../../source/renderer/app/components/governance/wallets/GovernanceWallets';
import type { WalletDelegationSummary } from '../../../source/renderer/app/components/governance/wallets/GovernanceWallets';
import type { EndedDelegation } from '../../../source/renderer/app/components/governance/wallets/EndedDelegationNotice';
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';
import type {
  DRepLookup,
  VoteDelegationState,
} from '../../../source/renderer/app/utils/governance/voteDelegationState';
import type { DRepIdentity } from '../../../source/common/types/governance.types';

const TOTAL_DREP_STAKE = new BigNumber('5257000000000000');

const drep = (
  suffix: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
): AppDRepDirectoryEntry => ({
  drepId: `drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k${String(
    suffix
  ).padStart(4, '0')}`,
  votingPower: new BigNumber('87000000000000'),
  status: 'active',
  drepActivity: 18,
  anchor: null,
  verifiedName: `Preview DRep ${suffix}`,
  doNotList: false,
  ...overrides,
});

const identity = (suffix: number): DRepIdentity => {
  const id = drep(suffix).drepId;
  return {
    raw: id,
    cip129: id,
    cip105: id,
    credentialHex: '00'.repeat(28),
    credentialType: 'key',
  };
};

const found = (entry: AppDRepDirectoryEntry): DRepLookup => ({
  status: 'found',
  entry,
});

const current = (suffix: number): VoteDelegationState => ({
  kind: 'current',
  voting: { kind: 'drep', drep: identity(suffix), source: 'ledger' },
  source: 'ledger',
});

// One row per wallet: the question this page answers is which wallets are
// delegated and to whom, which a column of cards answered badly.
const delegated: WalletDelegationSummary[] = [
  {
    walletId: 'wallet-1',
    walletName: 'Daily spending',
    state: current(1),
    lookup: found(drep(1)),
  },
  {
    walletId: 'wallet-2',
    walletName: 'Long-term savings',
    state: {
      kind: 'current',
      voting: { kind: 'abstain' },
      source: 'ledger',
    },
    lookup: { status: 'idle' },
  },
  {
    walletId: 'wallet-3',
    walletName: 'Cold storage',
    state: { kind: 'none' },
    lookup: { status: 'idle' },
  },
  {
    walletId: 'wallet-4',
    walletName: 'Delegated to a lapsing DRep',
    state: current(4),
    lookup: found(drep(4, { drepActivity: 3, verifiedName: null })),
  },
  {
    walletId: 'wallet-5',
    walletName: 'Delegated to an inactive DRep',
    state: current(5),
    lookup: found(drep(5, { status: 'inactive', drepActivity: 0 })),
  },
  {
    walletId: 'wallet-6',
    walletName: 'No confidence',
    state: {
      kind: 'current',
      voting: { kind: 'no_confidence' },
      source: 'ledger',
    },
    lookup: { status: 'idle' },
  },
];

const retired: WalletDelegationSummary = {
  walletId: 'wallet-7',
  walletName: 'Delegated to a retired DRep',
  state: { kind: 'ended', drep: identity(7), registeredAgain: false },
  lookup: { status: 'not_found' },
};

const retiredRegisteredAgain: WalletDelegationSummary = {
  walletId: 'wallet-8',
  walletName: 'DRep retired and registered again',
  state: { kind: 'ended', drep: identity(8), registeredAgain: true },
  lookup: found(drep(8)),
};

const notInRegistry: WalletDelegationSummary = {
  walletId: 'wallet-9',
  walletName: 'DRep missing from the registry',
  state: { kind: 'not_in_registry', drep: identity(9) },
  lookup: { status: 'not_found' },
};

const unavailable: WalletDelegationSummary = {
  walletId: 'wallet-10',
  walletName: 'DRep lookup failed',
  state: current(10),
  lookup: { status: 'failed' },
};

const wallets: WalletDelegationSummary[] = [
  ...delegated,
  retired,
  retiredRegisteredAgain,
  notInRegistry,
  unavailable,
];

const render = (
  walletList: WalletDelegationSummary[],
  endedDelegations: Array<EndedDelegation> = []
) => (
  <GovernanceShell activeTab={ROUTES.GOVERNANCE.DASHBOARD}>
    <GovernanceWallets
      wallets={walletList}
      endedDelegations={endedDelegations}
      favoriteDRepIds={new Set<string>()}
      totalDRepStake={TOTAL_DREP_STAKE}
      onToggleFavorite={action('onToggleFavorite')}
      onChangeDelegation={action('onChangeDelegation')}
      onViewDetails={action('onViewDetails')}
      onExternalLinkClick={action('onExternalLinkClick')}
    />
  </GovernanceShell>
);

storiesOf('Governance / Governance Center', module)
  .addDecorator((story) => (
    <StoryProvider>
      <StoryDecorator>{story()}</StoryDecorator>
    </StoryProvider>
  ))
  .add('Every delegation state', () => render(wallets))
  .add('Nothing delegated yet', () =>
    render(wallets.filter((w) => w.state.kind === 'none'))
  )
  .add('Delegation ended: DRep retired', () => render([retired]))
  .add('Delegation ended: DRep retired and registered again', () =>
    render([retiredRegisteredAgain])
  )
  .add('DRep not in the registry', () => render([notInRegistry]))
  .add('DRep data unavailable', () => render([unavailable]))
  .add('Ended delegation notice', () =>
    render(
      [retired, retiredRegisteredAgain, notInRegistry],
      [retired, retiredRegisteredAgain, notInRegistry].map(
        ({ walletId, walletName, state }, index) => ({
          walletId,
          walletName,
          state,
          heldReward: new BigNumber(index === 0 ? 3754.573734 : 0),
        })
      )
    )
  )
  .add('No wallets', () => render([]));
