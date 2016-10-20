// @flow
import React, { Component, PropTypes } from 'react';
import AppBar from 'material-ui/AppBar';
import { observer } from 'mobx-react';
import WalletNavigation from '../../components/wallet/navigation/WalletNavigation';
import styles from './WalletLayout.scss';

@observer(['store'])
export default class WalletLayout extends Component {
  static propTypes = {
    children: PropTypes.element.isRequired,
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

        {this.props.children}

      </div>
    );
  }
}
