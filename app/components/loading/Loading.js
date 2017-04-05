// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import LoadingSpinner from '../widgets/LoadingSpinner';
import cardanoLogo from '../../assets/images/cardano-logo.svg';
import cardanoLogoWhite from '../../assets/images/cardano-logo-white.svg';
import styles from './Loading.scss';

const messages = defineMessages({
  connecting: {
    id: 'loading.screen.connectingToNetworkMessage',
    defaultMessage: '!!!Connecting to network',
    description: 'Message "Connecting to network" on the loading screen.'
  },
  waitingForSyncToStart: {
    id: 'loading.screen.waitingForSyncToStart',
    defaultMessage: '!!!Connected - waiting for block syncing to start',
    description: 'Message "Connected - waiting for block syncing to start" on the loading screen.'
  },
  reconnecting: {
    id: 'loading.screen.reconnectingToNetworkMessage',
    defaultMessage: '!!!Network connection lost - reconnecting',
    description: 'Message "Network connection lost - reconnecting" on the loading screen.'
  },
  syncing: {
    id: 'loading.screen.syncingBlocksMessage',
    defaultMessage: '!!!Syncing blocks',
    description: 'Message "Syncing blocks" on the loading screen.'
  },
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.'
  }
});

@observer
export default class Loading extends Component {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static propTypes = {
    isConnecting: PropTypes.bool.isRequired,
    hasBeenConnected: PropTypes.bool.isRequired,
    isSyncing: PropTypes.bool.isRequired,
    hasBlockSyncingStarted: PropTypes.bool.isRequired,
    isLoadingWallets: PropTypes.bool.isRequired,
    syncPercentage: PropTypes.number.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      isConnecting, isSyncing, syncPercentage, isLoadingWallets, hasBeenConnected, hasBlockSyncingStarted
    } = this.props;
    const componentStyles = classNames([
      styles.component,
      isConnecting ? styles['is-connecting'] : null,
      isSyncing ? styles['is-syncing'] : null,
    ]);
    const logo = isConnecting ? cardanoLogoWhite : cardanoLogo;
    const connectingMessage = hasBeenConnected ? messages.reconnecting : messages.connecting;
    return (
      <div className={componentStyles}>
        <img className={styles.logo} src={logo} role="presentation" />
        {isConnecting && !hasBlockSyncingStarted && (
          <div className={styles.connecting}>
            <h1 className={styles.headline}>{intl.formatMessage(connectingMessage)}</h1>
          </div>
        )}
        {isConnecting && hasBlockSyncingStarted && (
          <div className={styles.connecting}>
            <h1 className={styles.headline}>{intl.formatMessage(messages.waitingForSyncToStart)}</h1>
          </div>
        )}
        {isSyncing && (
          <div className={styles.syncing}>
            <h1 className={styles.headline}>
              {intl.formatMessage(messages.syncing)} {syncPercentage.toFixed(2)}%
            </h1>
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
