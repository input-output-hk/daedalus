import React from 'react';
import { observer, inject } from 'mobx-react';
import { reaction } from 'mobx';
import type { IReactionDisposer } from 'mobx';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import DRepDirectory from '../../components/governance/drep-directory/DRepDirectory';
import GovernanceStore, {
  GovernanceRefreshState,
} from '../../stores/GovernanceStore';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';
import { pickDelegationFormReturnState } from './delegationFormState';
import { isSameDRep } from '../../utils/governance/isSameDRep';
import {
  effectiveCurrentVote,
  voteDelegationState,
} from '../../utils/governance/voteDelegationState';

interface Props extends RouteComponentProps {
  stores?: StoresMap;
}

@inject('stores')
@observer
class DRepDirectoryPage extends React.Component<Props> {
  syncReactionDisposer: IReactionDisposer | null = null;

  componentDidMount() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) {
      return;
    }

    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
    }

    // Favourites are pinned above the cohort, and the cohort is a random
    // twenty, so most favourites are not in it. Resolve them by id rather than
    // hoping they appear in the sample.
    governanceStore.ensureFavorites();

    // Replace the possibly-incomplete syncing snapshot exactly once when the
    // node reaches the tip; reaction fires only on the false -> true edge.
    this.syncReactionDisposer = reaction(
      () => stores?.networkStatus.isNodeInSync,
      (isNodeInSync) => {
        if (isNodeInSync) {
          governanceStore.refresh();
        }
      }
    );
  }

  componentWillUnmount() {
    if (this.syncReactionDisposer) {
      this.syncReactionDisposer();
      this.syncReactionDisposer = null;
    }
  }

  handleSelectForDelegation = (drepId: string) => {
    const governanceStore = this.props.stores?.governance;
    const inherited = pickDelegationFormReturnState(
      governanceStore?.delegationNavState
    );
    const entry = governanceStore?.lookupDRep(drepId) ?? null;
    governanceStore?.setDelegationNavState({
      ...inherited,
      selectedDRepId: drepId,
      selectedDRepVerifiedName: entry?.verifiedName ?? null,
      selectedDRepAnchorUrl: entry?.anchor?.url ?? null,
    });
    this.props.history.push(inherited?.from ?? ROUTES.GOVERNANCE.DELEGATE);
  };

  // For a list the wallet has not returned: the form needs no list.
  handleOpenDelegationForm = () => {
    const governanceStore = this.props.stores?.governance;
    governanceStore?.setDelegationNavState({
      selectedWalletId:
        governanceStore.delegationNavState?.selectedWalletId ?? null,
      voteType: 'drep',
    });
    this.props.history.push(ROUTES.GOVERNANCE.DELEGATE);
  };

  handleViewDetails = (drepId: string) => {
    this.props.history.push(`${ROUTES.GOVERNANCE.DREPS}/${drepId}`);
  };

  handleBackToDirectory = () => {
    this.props.history.push(ROUTES.GOVERNANCE.DREPS);
  };

  render() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;
    const networkStatus = stores?.networkStatus;

    if (!governanceStore || !networkStatus) return null;

    // Byron wallets carry no stake credential and cannot delegate voting
    // power, so a wallet set containing only those must not offer the action.
    const canDelegate = (stores?.wallets?.allWallets?.length ?? 0) > 0;

    const selectedWalletId =
      governanceStore.delegationNavState?.selectedWalletId;
    const selectedWallet = selectedWalletId
      ? (stores?.wallets?.allWallets?.find((w) => w.id === selectedWalletId) ??
        null)
      : null;
    // The ledger's delegation, not the wallet's history: after a DRep retires
    // the history still names it, and a DRep that registered again must be
    // choosable.
    const currentDRep = selectedWallet
      ? effectiveCurrentVote(
          voteDelegationState(selectedWallet, (id) =>
            governanceStore.drepLookup(id)
          )
        )
      : null;
    const isCurrentDRep =
      currentDRep?.kind === 'drep'
        ? (entry: { drepId: string }) => isSameDRep(entry.drepId, currentDRep)
        : undefined;

    return (
      <DRepDirectory
        suggestedDReps={governanceStore.suggestedDReps}
        allDReps={governanceStore.allDReps}
        cohortCriteria={governanceStore.cohortCriteria}
        onCohortCriteriaChange={(criteria) =>
          governanceStore.setCohortCriteria(criteria)
        }
        relaxedCohortCriteria={governanceStore.cohortPool.relaxed}
        favoriteDRepIds={governanceStore.favoriteDRepIds}
        onToggleFavorite={(drepId) => governanceStore.toggleFavorite(drepId)}
        favoriteEntries={governanceStore.favoriteEntries}
        listViewMode={stores.profile?.getListViewMode?.('drepDirectory')}
        onListViewModeChange={(mode) =>
          stores.profile?.setListViewMode?.('drepDirectory', mode)
        }
        totalDRepStake={governanceStore.drepSummary?.totalDRepStake ?? null}
        epochLength={networkStatus.epochLength}
        slotLength={networkStatus.slotLength}
        view={
          this.props.location.pathname.startsWith(ROUTES.GOVERNANCE.FAVORITES)
            ? 'favorites'
            : 'directory'
        }
        onBackToDirectory={this.handleBackToDirectory}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        isNodeInSync={networkStatus.isNodeInSync}
        syncProgress={networkStatus.syncProgress}
        onRefresh={() => governanceStore.refresh()}
        onReroll={() => governanceStore.rerollCohort()}
        onLoadAllDReps={() => governanceStore.loadAllDReps()}
        isCurrentDRep={isCurrentDRep}
        canDelegate={canDelegate}
        onSelectForDelegation={this.handleSelectForDelegation}
        onOpenDelegationForm={this.handleOpenDelegationForm}
        onViewDetails={this.handleViewDetails}
      />
    );
  }
}

export default withRouter(DRepDirectoryPage);
