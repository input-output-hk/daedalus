import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DappConnectionsSettings from '../../../components/settings/categories/DappConnectionsSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores')
@observer
class DappConnectionsSettingsPage extends Component<InjectedProps> {
  static defaultProps = { stores: null, actions: null };

  componentDidMount() {
    this.props.stores.dapp.refreshConnections();
  }

  render() {
    const { dapp, wallets } = this.props.stores;
    return (
      <DappConnectionsSettings
        connections={dapp.connections.map((grant) => ({
          grant,
          walletName:
            wallets.getWalletById(grant.walletId)?.name || grant.walletId,
        }))}
        corrupt={dapp.connectionsCorrupt}
        loading={dapp.isManagingConnections}
        failed={dapp.connectionActionFailed}
        onDisconnect={(grant) => dapp.disconnectConnection(grant)}
        onForget={(grant) => dapp.forgetConnection(grant)}
        onRevoke={(grant, scope) => dapp.revokeConnectionScope(grant, scope)}
        onRepair={() => dapp.repairConnections()}
      />
    );
  }
}

export default DappConnectionsSettingsPage;
