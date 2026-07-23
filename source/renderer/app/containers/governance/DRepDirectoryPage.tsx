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
    // Combine the inherited { from, selectedWalletId, voteType } with the
    // row's DRep ID; the handoff travels only through location.state.
    const inherited = pickDelegationFormReturnState(this.props.location.state);
    this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE, {
      ...inherited,
      selectedDRepId: drepId,
    });
  };

  render() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;
    const networkStatus = stores?.networkStatus;

    if (!governanceStore || !networkStatus) return null;

    return (
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        isNodeInSync={networkStatus.isNodeInSync}
        syncProgress={networkStatus.syncProgress}
        votingPowerState={governanceStore.votingPowerState}
        onRefresh={() => governanceStore.refresh()}
        onSelectForDelegation={this.handleSelectForDelegation}
      />
    );
  }
}

export default withRouter(DRepDirectoryPage);
