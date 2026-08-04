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
    const inherited = pickDelegationFormReturnState(governanceStore?.delegationNavState);
    const entry =
      governanceStore?.suggestedDReps.find((e) => e.drepId === drepId) ??
      governanceStore?.allDReps.find((e) => e.drepId === drepId);
    governanceStore?.setDelegationNavState({
      ...inherited,
      selectedDRepId: drepId,
      selectedDRepVerifiedName: entry?.verifiedName ?? null,
      selectedDRepAnchorUrl: entry?.anchor?.url ?? null,
    });
    const isSentinel = drepId === 'abstain' || drepId === 'no_confidence';
    if (!isSentinel && !governanceStore?.favoriteDRepIds.has(drepId)) {
      governanceStore?.toggleFavorite(drepId);
    }
    this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE);
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

    const canDelegate = (stores?.wallets?.all?.length ?? 0) > 0;

    return (
      <DRepDirectory
        suggestedDReps={governanceStore.suggestedDReps}
        allDReps={governanceStore.allDReps}
        allDRepsRefreshState={governanceStore.allDRepsRefreshState}
        favoriteDRepIds={governanceStore.favoriteDRepIds}
        onToggleFavorite={(drepId) => governanceStore.toggleFavorite(drepId)}
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
        onReroll={() => governanceStore.fetchSuggestedDReps()}
        onLoadAllDReps={() => governanceStore.loadAllDReps()}
        canDelegate={canDelegate}
        onSelectForDelegation={this.handleSelectForDelegation}
        onViewDetails={this.handleViewDetails}
      />
    );
  }
}

export default withRouter(DRepDirectoryPage);
