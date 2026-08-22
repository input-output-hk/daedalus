import React from 'react';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
import GovernanceWallets from '../../../../source/renderer/app/components/governance/wallets/GovernanceWallets';
import type { WalletDelegationSummary } from '../../../../source/renderer/app/components/governance/wallets/GovernanceWallets';
import DRepDirectory from '../../../../source/renderer/app/components/governance/drep-directory/DRepDirectory';
import DRepDetail from '../../../../source/renderer/app/components/governance/drep-detail/DRepDetail';
import { GovernanceRefreshState } from '../../../../source/renderer/app/stores/GovernanceStore';
import type {
  AppDRepDetail,
  AppDRepDirectoryEntry,
} from '../../../../source/renderer/app/stores/GovernanceStore';
import { ROUTES } from '../../../../source/renderer/app/routes-config';

// Measured against mainnet on 2026-08-20: 1,062 registered DReps holding
// 5.257B ADA between them.
export const TOTAL_DREP_STAKE = new BigNumber('5257000000000000');

// Preprod and mainnet: 432,000 slots of one second, so an epoch is five days.
export const EPOCH_LENGTH = 432000;
export const SLOT_LENGTH = 1;

export type GovernancePageState = {
  entries?: AppDRepDirectoryEntry[];
  favoriteEntries?: AppDRepDirectoryEntry[];
  favoriteDRepIds?: Set<string>;
  wallets?: WalletDelegationSummary[];
  detail?: AppDRepDetail | null;
  refreshState?: GovernanceRefreshState;
  error?: { message: string; type: string } | null;
  isNodeInSync?: boolean;
  syncProgress?: number | null;
  listViewMode?: 'cards' | 'table';
  isFavorite?: boolean;
};

/**
 * The governance section's page for a given tab.
 *
 * Every story renders through this, so a tab click lands on the same screen it
 * would land on in the application, and a story that is about one screen still
 * shows the others truthfully rather than showing nothing.
 */
/**
 * Enough of a DRep to populate the tabs a story is not about. Deliberately
 * small: these fixtures exist so navigation lands somewhere real, not to
 * stand in for the story that owns that screen.
 */
const FALLBACK_ENTRY: AppDRepDirectoryEntry = {
  drepId: 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0001',
  votingPower: new BigNumber('87000000000000'),
  status: 'active',
  drepActivity: 18,
  anchor: null,
  verifiedName: 'Preview DRep',
  doNotList: false,
};

const FALLBACK_WALLETS: WalletDelegationSummary[] = [
  {
    walletId: 'wallet-1',
    walletName: 'Daily spending',
    currentDRep: {
      kind: 'drep',
      drep: { raw: FALLBACK_ENTRY.drepId } as any,
      source: 'onchain',
    },
    drepEntry: FALLBACK_ENTRY,
  },
  {
    walletId: 'wallet-2',
    walletName: 'Long-term savings',
    currentDRep: null,
    drepEntry: null,
  },
];

export function renderGovernancePage(
  route: string,
  state: GovernancePageState = {}
) {
  const {
    entries = [FALLBACK_ENTRY],
    favoriteEntries = [],
    favoriteDRepIds = new Set<string>(),
    wallets = FALLBACK_WALLETS,
    detail = null,
    refreshState = GovernanceRefreshState.Loaded,
    error = null,
    isNodeInSync = true,
    syncProgress = 100,
    listViewMode,
    isFavorite = false,
  } = state;

  if (route === ROUTES.GOVERNANCE.DASHBOARD) {
    return (
      <GovernanceWallets
        wallets={wallets}
        favoriteDRepIds={favoriteDRepIds}
        totalDRepStake={TOTAL_DREP_STAKE}
        onToggleFavorite={action('onToggleFavorite')}
        onChangeDelegation={action('onChangeDelegation')}
        onChooseDRep={action('onChooseDRep')}
        onViewDetails={action('onViewDetails')}
      />
    );
  }

  if (route === ROUTES.GOVERNANCE.DREP_DETAIL) {
    return (
      <DRepDetail
        entry={detail}
        refreshState={refreshState}
        totalDRepStake={TOTAL_DREP_STAKE}
        epochLength={EPOCH_LENGTH}
        slotLength={SLOT_LENGTH}
        isFavorite={isFavorite}
        onBackToDirectory={action('onBackToDirectory')}
        onOpenExternalLink={action('onOpenExternalLink')}
        onSelectForDelegation={action('onSelectForDelegation')}
        onToggleFavorite={action('onToggleFavorite')}
      />
    );
  }

  const isFavoritesView = route === ROUTES.GOVERNANCE.FAVORITES;

  return (
    <DRepDirectory
      suggestedDReps={entries}
      allDReps={entries}
      favoriteDRepIds={favoriteDRepIds}
      favoriteEntries={favoriteEntries}
      onToggleFavorite={action('onToggleFavorite')}
      view={isFavoritesView ? 'favorites' : 'directory'}
      onBackToDirectory={action('onBackToDirectory')}
      error={error}
      isNodeInSync={isNodeInSync}
      lastFetchedAt={Date.now() - 3 * 60 * 1000}
      onRefresh={action('onRefresh')}
      onReroll={action('onReroll')}
      onLoadAllDReps={action('onLoadAllDReps')}
      refreshState={refreshState}
      totalDRepStake={TOTAL_DREP_STAKE}
      epochLength={EPOCH_LENGTH}
      slotLength={SLOT_LENGTH}
      listViewMode={listViewMode}
      onListViewModeChange={action('onListViewModeChange')}
      onSelectForDelegation={action('onSelectForDelegation')}
      onViewDetails={action('onViewDetails')}
      syncProgress={syncProgress}
    />
  );
}
