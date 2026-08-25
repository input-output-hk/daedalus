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
import type { AppDRepDirectoryEntry } from '../../../source/renderer/app/stores/GovernanceStore';

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

// One row per wallet: the question this page answers is which wallets are
// delegated and to whom, which a column of cards answered badly.
const wallets: WalletDelegationSummary[] = [
  {
    walletId: 'wallet-1',
    walletName: 'Daily spending',
    currentDRep: {
      kind: 'drep',
      drep: { raw: drep(1).drepId } as any,
      source: 'onchain',
    },
    drepEntry: drep(1),
  },
  {
    walletId: 'wallet-2',
    walletName: 'Long-term savings',
    currentDRep: { kind: 'abstain' },
    drepEntry: null,
  },
  {
    walletId: 'wallet-3',
    walletName: 'Cold storage',
    currentDRep: null,
    drepEntry: null,
  },
  {
    walletId: 'wallet-4',
    walletName: 'Delegated to a lapsing DRep',
    currentDRep: {
      kind: 'drep',
      drep: { raw: drep(4).drepId } as any,
      source: 'onchain',
    },
    drepEntry: drep(4, { drepActivity: 3, verifiedName: null }),
  },
  {
    walletId: 'wallet-5',
    walletName: 'Delegated to an inactive DRep',
    currentDRep: {
      kind: 'drep',
      drep: { raw: drep(5).drepId } as any,
      source: 'onchain',
    },
    drepEntry: drep(5, { status: 'inactive', drepActivity: 0 }),
  },
  {
    walletId: 'wallet-6',
    walletName: 'No confidence',
    currentDRep: { kind: 'no_confidence' },
    drepEntry: null,
  },
];

const render = (walletList: WalletDelegationSummary[]) => (
  <GovernanceShell activeTab={ROUTES.GOVERNANCE.DASHBOARD}>
    <GovernanceWallets
      wallets={walletList}
      favoriteDRepIds={new Set<string>()}
      totalDRepStake={TOTAL_DREP_STAKE}
      onToggleFavorite={action('onToggleFavorite')}
      onChangeDelegation={action('onChangeDelegation')}
      onChooseDRep={action('onChooseDRep')}
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
    render(wallets.filter((w) => w.currentDRep == null))
  )
  .add('No wallets', () => render([]));
