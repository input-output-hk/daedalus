// @flow
import React, { Component } from 'react';
import AppBar from 'material-ui/AppBar';
import { observer } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import WalletNavigation from '../../components/wallet/navigation/WalletNavigation';
import wallet from '../../stores/walletStore';
import styles from './WalletSendPage.scss';
import walletSendFormValidator from '../../validators/walletSendFormValidator';

@observer
export default class WalletSendPage extends Component {
  render() {
    return (
      <div className={styles.component}>

        <AppBar
          title="Daedalus"
          className={styles.appBar}
          showMenuIconButton={false}
        />

        <WalletNavigation wallet={wallet} />

        <WalletSendForm validator={walletSendFormValidator} />

      </div>
    );
  }
}
