import React from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import GovernanceWallets from '../../components/governance/wallets/GovernanceWallets';
import type { WalletDelegationSummary } from '../../components/governance/wallets/GovernanceWallets';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';

interface Props extends RouteComponentProps {
  stores?: StoresMap;
}

@inject('stores')
@observer
class GovernanceWalletsPage extends React.Component<Props> {
  _ensureAllDReps() {
    const { wallets, governance } = this.props.stores ?? {};
    if (!governance) return;
    for (const wallet of wallets?.all ?? []) {
      if (wallet.currentDRep?.kind === 'drep') {
        const drepId =
          wallet.currentDRep.drep.cip129 ?? wallet.currentDRep.drep.raw;
        governance.ensureDRep(drepId);
      }
    }
  }

  componentDidMount() {
    this._ensureAllDReps();
  }

  componentDidUpdate() {
    this._ensureAllDReps();
  }

  handleChangeDelegation = (walletId: string) => {
    const { governance } = this.props.stores ?? {};
    governance?.setDelegationNavState({
      selectedWalletId: walletId,
      voteType: 'drep',
    });
    this.props.history.push(ROUTES.GOVERNANCE.DREPS);
  };

  handleChooseDRep = () => {
    this.props.history.push(ROUTES.GOVERNANCE.DREPS);
  };

  handleViewDetails = (drepId: string, walletId: string) => {
    const { governance } = this.props.stores ?? {};
    governance?.setDelegationNavState({ selectedWalletId: walletId });
    this.props.history.push(`${ROUTES.GOVERNANCE.DREPS}/${drepId}`);
  };

  render() {
    const { wallets, governance } = this.props.stores ?? {};

    const allWallets: WalletDelegationSummary[] = (wallets?.all ?? []).map(
      (w) => {
        const drepId =
          w.currentDRep?.kind === 'drep'
            ? (w.currentDRep.drep.cip129 ?? w.currentDRep.drep.raw)
            : null;
        return {
          walletId: w.id,
          walletName: w.name,
          currentDRep: w.currentDRep,
          drepEntry: drepId ? (governance?.lookupDRep(drepId) ?? null) : null,
        };
      }
    );

    return (
      <GovernanceWallets
        wallets={allWallets}
        favoriteDRepIds={governance?.favoriteDRepIds ?? new Set()}
        onToggleFavorite={(drepId) => governance?.toggleFavorite(drepId)}
        onChangeDelegation={this.handleChangeDelegation}
        onChooseDRep={this.handleChooseDRep}
        onViewDetails={this.handleViewDetails}
      />
    );
  }
}

export default withRouter(GovernanceWalletsPage);
