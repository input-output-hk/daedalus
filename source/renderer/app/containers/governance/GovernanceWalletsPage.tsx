import React from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import GovernanceWallets from '../../components/governance/wallets/GovernanceWallets';
import type { WalletDelegationSummary } from '../../components/governance/wallets/GovernanceWallets';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';
import {
  drepLookupKey,
  drepOfState,
  voteDelegationState,
} from '../../utils/governance/voteDelegationState';
import type { DRepLookup } from '../../utils/governance/voteDelegationState';

interface Props extends RouteComponentProps {
  stores?: StoresMap;
}

const IDLE: DRepLookup = { status: 'idle' };

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
      const drep = drepOfState(voteDelegationState(wallet, () => IDLE));
      // Lookups go through the store, which sends one per DRep and remembers
      // the answer, so a screen that updates often adds no request.
      if (drep) governance.resolveDRep(drepLookupKey(drep));
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
    this.props.history.push(ROUTES.GOVERNANCE.DELEGATE);
  };

  handleViewDetails = (drepId: string, walletId: string) => {
    const { governance } = this.props.stores ?? {};
    governance?.setDelegationNavState({ selectedWalletId: walletId });
    this.props.history.push(`${ROUTES.GOVERNANCE.DREPS}/${drepId}`);
  };

  render() {
    const { app, wallets, governance } = this.props.stores ?? {};

    const lookupOf = (drepId: string): DRepLookup =>
      governance?.drepLookup(drepId) ?? IDLE;
    const allWallets: WalletDelegationSummary[] = (
      wallets?.allWallets ?? []
    ).map((w) => {
      const state = voteDelegationState(w, lookupOf);
      const drep = drepOfState(state);
      return {
        walletId: w.id,
        walletName: w.name,
        state,
        lookup: drep ? lookupOf(drepLookupKey(drep)) : IDLE,
      };
    });

    return (
      <GovernanceWallets
        wallets={allWallets}
        endedDelegations={governance?.endedDelegations ?? []}
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
