import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import WalletDbDamageView from '../../components/loading/wallet-db-damage/WalletDbDamageView';
import styles from '../../components/loading/mithril/MithrilBootstrap.scss';

@inject('stores', 'actions')
@observer
class WalletDbDamageContainer extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const {
      walletDbDamage,
      walletDbRepairView,
      repairWalletDatabases,
      dismissWalletDbDamage,
    } = this.props.stores.backend;
    if (walletDbRepairView == null) return null;

    return (
      <div className={styles.card}>
        <WalletDbDamageView
          view={walletDbRepairView}
          damagedCount={walletDbDamage?.files.length ?? 0}
          results={walletDbDamage?.results ?? []}
          backupDir={walletDbDamage?.backupDir ?? null}
          onRepair={repairWalletDatabases}
          onDismiss={dismissWalletDbDamage}
        />
      </div>
    );
  }
}

export default WalletDbDamageContainer;
