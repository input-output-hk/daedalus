// @flow
import React, { Component, PropTypes } from 'react';
import AppBar from 'material-ui/AppBar';
import { observer } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import WalletNavigation from '../../components/wallet/navigation/WalletNavigation';
import styles from './WalletSendPage.scss';
import walletSendFormValidator from '../../validators/walletSendFormValidator';

@observer(['store'])
export default class WalletSendPage extends Component {
  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired
    })
  };
  render() {
    return (
      <div className={styles.component}>

        <AppBar
          title="Daedalus"
          className={styles.appBar}
          showMenuIconButton={false}
        />

        <WalletNavigation wallet={this.props.store.wallet} />

        <WalletSendForm validator={walletSendFormValidator} />

      </div>
    );
  }
}
