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
import type { AppDRepDetail } from '../../stores/GovernanceStore';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';
import { pickDelegationFormReturnState } from './delegationFormState';
import { isSameDRep } from '../../utils/governance/isSameDRep';

interface Props extends RouteComponentProps<{ drepId: string }> {
  stores?: StoresMap;
}

interface State {
  detail: AppDRepDetail | null;
  detailRefreshState: GovernanceRefreshState;
}

@inject('stores')
@observer
class DRepDetailPage extends React.Component<Props, State> {
  state: State = {
    detail: null,
    detailRefreshState: GovernanceRefreshState.Loading,
  };

  syncReactionDisposer: IReactionDisposer | null = null;

  async componentDidMount() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) return;

    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
    }

    this.syncReactionDisposer = reaction(
      () => stores?.networkStatus.isNodeInSync,
      (isNodeInSync) => {
        if (isNodeInSync) governanceStore.refresh();
      }
    );

    const { drepId } = this.props.match.params;
    try {
      const detail = await governanceStore.fetchDRep(drepId);
      this.setState({
        detail,
        detailRefreshState: GovernanceRefreshState.Loaded,
      });
    } catch {
      this.setState({ detailRefreshState: GovernanceRefreshState.Failed });
    }
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
    const { detail } = this.state;
    governanceStore?.setDelegationNavState({
      ...inherited,
      selectedDRepId: drepId,
      selectedDRepVerifiedName: detail?.verifiedName ?? null,
      selectedDRepAnchorUrl: detail?.anchor?.url ?? null,
    });
    if (!governanceStore?.favoriteDRepIds.has(drepId)) {
      governanceStore?.toggleFavorite(drepId);
    }
    this.props.history.push(inherited?.from ?? ROUTES.GOVERNANCE.DELEGATE);
  };

  handleBackToDirectory = () => {
    this.props.history.push(ROUTES.GOVERNANCE.DREPS);
  };

  render() {
    const { stores } = this.props;
    if (!stores?.governance) return null;

    const canDelegate = (stores?.wallets?.allWallets?.length ?? 0) > 0;
    const { detail } = this.state;
    const isFavorite =
      detail != null && stores.governance.favoriteDRepIds.has(detail.drepId);

    const selectedWalletId =
      stores.governance.delegationNavState?.selectedWalletId;
    const selectedWallet = selectedWalletId
      ? (stores.wallets?.allWallets?.find((w) => w.id === selectedWalletId) ??
        null)
      : null;
    const isCurrentDRep =
      detail != null && selectedWallet?.currentDRep != null
        ? isSameDRep(detail.drepId, selectedWallet.currentDRep)
        : false;

    return (
      <DRepDetail
        entry={detail}
        totalDRepStake={stores.governance?.drepSummary?.totalDRepStake ?? null}
        epochLength={stores.networkStatus?.epochLength ?? null}
        slotLength={stores.networkStatus?.slotLength ?? null}
        refreshState={this.state.detailRefreshState}
        onOpenExternalLink={stores.app.openExternalLink}
        network={stores.app.environment?.network}
        canDelegate={canDelegate}
        isFavorite={isFavorite}
        isCurrentDRep={isCurrentDRep}
        onSelectForDelegation={this.handleSelectForDelegation}
        onToggleFavorite={(drepId) => stores.governance.toggleFavorite(drepId)}
        onBackToDirectory={this.handleBackToDirectory}
      />
    );
  }
}

export default withRouter(DRepDetailPage);
