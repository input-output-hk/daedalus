import React from 'react';
import { observer, inject } from 'mobx-react';
import { reaction } from 'mobx';
import type { IReactionDisposer } from 'mobx';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import DRepDetail from '../../components/governance/drep-detail/DRepDetail';
import GovernanceStore, {
  GovernanceRefreshState,
} from '../../stores/GovernanceStore';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';
import { pickDelegationFormReturnState } from './delegationFormState';

interface Props extends RouteComponentProps<{ drepId: string }> {
  stores?: StoresMap;
}

@inject('stores')
@observer
class DRepDetailPage extends React.Component<Props> {
  syncReactionDisposer: IReactionDisposer | null = null;
  anchorReactionDisposer: IReactionDisposer | null = null;

  componentDidMount() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) {
      return;
    }

    // Deep links and restarts land here with an empty store; the same
    // refresh contract as the directory keeps the detail self-sufficient.
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

    // Deep links mount before the list resolves, so fireImmediately plus the
    // reaction covers both arrival orders.
    this.anchorReactionDisposer = reaction(
      () =>
        governanceStore.drepIndex.get(this.props.match.params.drepId)?.anchor ??
        null,
      (anchor) => {
        if (anchor) {
          governanceStore.fetchAnchorContent(
            this.props.match.params.drepId,
            anchor
          );
        }
      },
      { fireImmediately: true }
    );
  }

  componentWillUnmount() {
    if (this.syncReactionDisposer) {
      this.syncReactionDisposer();
      this.syncReactionDisposer = null;
    }
    if (this.anchorReactionDisposer) {
      this.anchorReactionDisposer();
      this.anchorReactionDisposer = null;
    }
  }

  handleSelectForDelegation = (drepId: string) => {
    // The inherited { from, selectedWalletId, voteType } plus the id return
    // to the form through location.state only — never query params or stores.
    const inherited = pickDelegationFormReturnState(this.props.location.state);
    this.props.history.push(inherited?.from ?? ROUTES.VOTING.GOVERNANCE, {
      ...inherited,
      selectedDRepId: drepId,
    });
  };

  handleBackToDirectory = () => {
    // Keep the round-trip state alive so a row-select after backing out
    // still restores the user's wallet and vote type.
    this.props.history.push(
      ROUTES.GOVERNANCE.DREPS,
      pickDelegationFormReturnState(this.props.location.state)
    );
  };

  render() {
    const { stores, match } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) return null;

    // The route param is used untransformed: bech32 ids contain no characters
    // that URL-encode, so byte-equality with the directory entry holds.
    const { drepId } = match.params;

    return (
      <DRepDetail
        entry={governanceStore.drepIndex.get(drepId) ?? null}
        cohort={governanceStore.cohortContext}
        refreshState={governanceStore.refreshState}
        votingPowerState={governanceStore.votingPowerState}
        anchorState={governanceStore.anchorStateByDRepId.get(drepId) ?? null}
        onOpenExternalLink={stores.app.openExternalLink}
        onSelectForDelegation={this.handleSelectForDelegation}
        onBackToDirectory={this.handleBackToDirectory}
      />
    );
  }
}

export default withRouter(DRepDetailPage);
