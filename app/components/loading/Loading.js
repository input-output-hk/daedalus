// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SystemTimeErrorOverlay from './SystemTimeErrorOverlay';
import LoadingSpinner from '../widgets/LoadingSpinner';
import daedalusLogo from '../../assets/images/daedalus-logo-loading-grey.inline.svg';
import styles from './Loading.scss';
import type { ReactIntlMessage } from '../../types/i18nTypes';
import environment from '../../environment';

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
});

type Props = {
  currencyIcon: string,
  apiIcon: string,
  isConnecting: boolean,
  hasBeenConnected: boolean,
  hasBlockSyncingStarted: boolean,
  isSyncing: boolean,
  syncPercentage: number,
  isLoadingDataForNextScreen: boolean,
  loadingDataForNextScreenMessage: ReactIntlMessage,
  hasLoadedCurrentLocale: boolean,
  hasLoadedCurrentTheme: boolean,
  localTimeDifference: number,
  allowedTimeDifference: number,
  currentLocale: string,
};

@observer
export default class Loading extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      currencyIcon,
      apiIcon,
      isConnecting,
      isSyncing,
      syncPercentage,
      isLoadingDataForNextScreen,
      loadingDataForNextScreenMessage,
      hasBeenConnected,
      hasBlockSyncingStarted,
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      localTimeDifference,
      allowedTimeDifference,
      currentLocale,
    } = this.props;

    const componentStyles = classNames([
      styles.component,
      hasLoadedCurrentTheme ? null : styles['is-loading-theme'],
      isConnecting ? styles['is-connecting'] : null,
      isSyncing ? styles['is-syncing'] : null,
    ]);
    const daedalusLogoStyles = classNames([
      styles.daedalusLogo,
      isConnecting ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const currencyLogoStyles = classNames([
      styles[`${environment.API}-logo`],
      isConnecting ? styles.connectingLogo : styles.syncingLogo,
    ]);
    const apiLogoStyles = classNames([
      styles[`${environment.API}-apiLogo`],
      isConnecting ? styles.connectingLogo : styles.syncingLogo,
    ]);

    const daedalusLoadingLogo = daedalusLogo;
    const currencyLoadingLogo = currencyIcon;
    const apiLoadingLogo = apiIcon;

    const connectingMessage = hasBeenConnected ? messages.reconnecting : messages.connecting;

    return (
      <div className={componentStyles}>
        <div className={styles.logos}>
          <SvgInline svg={currencyLoadingLogo} className={currencyLogoStyles} />
          <SvgInline svg={daedalusLoadingLogo} className={daedalusLogoStyles} />
          <SvgInline svg={apiLoadingLogo} className={apiLogoStyles} />
        </div>
        {hasLoadedCurrentLocale && (
          <div>
            {isConnecting && !hasBlockSyncingStarted && (
              <div className={styles.connecting}>
                <h1 className={styles.headline}>
                  {intl.formatMessage(connectingMessage)}
                </h1>
              </div>
            )}
            {isConnecting && hasBlockSyncingStarted && (
              <div className={styles.connecting}>
                <h1 className={styles.headline}>
                  {intl.formatMessage(messages.waitingForSyncToStart)}
                </h1>
              </div>
            )}
            {isSyncing && (
              <div className="syncingWrapper">
                <div className={styles.syncing}>
                  <h1 className={styles.headline}>
                    {intl.formatMessage(messages.syncing)} {syncPercentage.toFixed(2)}%
                  </h1>
                </div>
                {(localTimeDifference > allowedTimeDifference) && (
                  <SystemTimeErrorOverlay
                    localTimeDifference={localTimeDifference}
                    currentLocale={currentLocale}
                  />
                )}
              </div>
            )}
            {!isSyncing && !isConnecting && isLoadingDataForNextScreen && (
              <div className={styles.syncing}>
                <h1 className={styles.headline}>
                  {intl.formatMessage(loadingDataForNextScreenMessage)}
                </h1>
                <LoadingSpinner />
              </div>
            )}
          </div>
        )}
      </div>
    );
  }

}
