import React from 'react';
import { observer, inject } from 'mobx-react';
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
  componentDidMount() {
    const governanceStore: GovernanceStore | undefined =
      this.props.stores?.governance;

    if (!governanceStore) {
      return;
    }

    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
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

    if (!governanceStore) return null;

    return (
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        onRefresh={() => governanceStore.refresh()}
        onSelectForDelegation={this.handleSelectForDelegation}
      />
    );
  }
}

export default withRouter(DRepDirectoryPage);
