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
    // Shelley wallets only. A Byron wallet has no stake credential, so it
    // cannot delegate voting power at all, and listing it here offered an
    // action that could never be completed.
    for (const wallet of wallets?.allWallets ?? []) {
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

  handleViewDetails = (drepId: string, walletId: string) => {
    const { governance } = this.props.stores ?? {};
    governance?.setDelegationNavState({ selectedWalletId: walletId });
    this.props.history.push(`${ROUTES.GOVERNANCE.DREPS}/${drepId}`);
  };

  render() {
    const { app, wallets, governance } = this.props.stores ?? {};

    const allWallets: WalletDelegationSummary[] = (
      wallets?.allWallets ?? []
    ).map((w) => {
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
    });

    return (
      <GovernanceWallets
        wallets={allWallets}
        favoriteDRepIds={governance?.favoriteDRepIds ?? new Set()}
        totalDRepStake={governance?.drepSummary?.totalDRepStake ?? null}
        onToggleFavorite={(drepId) => governance?.toggleFavorite(drepId)}
        onChangeDelegation={this.handleChangeDelegation}
        onViewDetails={this.handleViewDetails}
        onExternalLinkClick={(url, event) => app?.openExternalLink(url, event)}
      />
    );
  }
}

export default withRouter(GovernanceWalletsPage);
