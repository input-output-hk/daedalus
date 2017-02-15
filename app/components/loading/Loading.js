// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import LoadingSpinner from '../widgets/LoadingSpinner';
import cardanoLogo from '../../assets/images/cardano-logo.svg';
import cardanoLogoWhite from '../../assets/images/cardano-logo-white.svg';
import styles from './Loading.scss';

@observer
export default class Loading extends Component {

  static propTypes = {
    isConnecting: PropTypes.bool.isRequired,
    isSyncing: PropTypes.bool.isRequired,
    isLoadingWallets: PropTypes.bool.isRequired,
    syncPercentage: PropTypes.number.isRequired,
  };

  render() {
    const { isConnecting, isSyncing, syncPercentage, isLoadingWallets } = this.props;
    const componentStyles = classNames([
      styles.component,
      isConnecting ? styles['is-connecting'] : null,
      isSyncing ? styles['is-syncing'] : null,
    ]);
    const logo = isConnecting ? cardanoLogoWhite : cardanoLogo;
    return (
      <div className={componentStyles}>
        <img className={styles.logo} src={logo} role="presentation" />
        {isConnecting && (
          <div className={styles.connecting}>
            <h1 className={styles.headline}>Connecting network</h1>
          </div>
        )}
        {isSyncing && (
          <div className={styles.syncing}>
            <h1 className={styles.headline}>Syncing blocks {syncPercentage.toFixed(0)}%</h1>
          </div>
        )}{!isSyncing && !isConnecting && isLoadingWallets && (
          <div className={styles.syncing}>
            <h1 className={styles.headline}>Loading wallet data</h1>
            <LoadingSpinner />
          </div>
        )}
      </div>
    );
  }
}
