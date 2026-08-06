import React from 'react';
import { observer, inject } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import GovernanceDashboard from '../../components/governance/dashboard/GovernanceDashboard';
import type { WalletDelegationSummary } from '../../components/governance/dashboard/GovernanceDashboard';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';

interface Props extends RouteComponentProps {
  stores?: StoresMap;
}

@inject('stores')
@observer
class GovernanceDashboardPage extends React.Component<Props> {
  handleChangeDelegation = (walletId: string) => {
    const { governance } = this.props.stores ?? {};
    governance?.setDelegationNavState({ selectedWalletId: walletId });
    this.props.history.push(ROUTES.VOTING.GOVERNANCE);
  };

  handleChooseDRep = () => {
    this.props.history.push(ROUTES.GOVERNANCE.DREPS);
  };

  render() {
    const wallets = this.props.stores?.wallets?.all ?? [];

    const delegatingWallets: WalletDelegationSummary[] = wallets
      .filter((w) => w.currentDRep != null)
      .map((w) => ({
        walletId: w.id,
        walletName: w.name,
        currentDRep: w.currentDRep!,
      }));

    return (
      <GovernanceDashboard
        wallets={delegatingWallets}
        onChangeDelegation={this.handleChangeDelegation}
        onChooseDRep={this.handleChooseDRep}
      />
    );
  }
}

export default withRouter(GovernanceDashboardPage);
