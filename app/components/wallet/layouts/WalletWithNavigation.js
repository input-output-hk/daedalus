// @flow
import React, { Component, PropTypes } from 'react';
import AppBar from 'material-ui/AppBar';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

@observer
export default class WalletLayoutWithNavigation extends Component {

  static propTypes = {
    children: PropTypes.element.isRequired,
    wallet: React.PropTypes.shape({
      name: PropTypes.string.isRequired,
      amount: PropTypes.number.isRequired,
      currency: PropTypes.string.isRequired,
    }),
  };

  render() {
    const { wallet, children } = this.props;
    return (
      <div className={styles.component}>

        <AppBar
          title="Daedalus"
          className={styles.appBar}
          showMenuIconButton={false}
        />

        <WalletNavigation wallet={wallet} />

        {children}

      </div>
    );
  }
}
