// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import ReactModal from 'react-modal';
import { capitalize } from 'lodash';
import { networkPrettyNames } from '../../../../common/types/environment.types';
import DaedalusDiagnostics from '../../components/status/DaedalusDiagnostics';
import styles from './DaedalusDiagnosticsDialog.scss';
import GenericNotification from '../../components/notifications/GenericNotification';
import { COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION } from '../../config/timingConfig';
import { formattedBytesToSize } from '../../utils/formatters';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

export const messages = defineMessages({
  stateDirectoryCopyNotificationMessage: {
    id: 'daedalus.diagnostics.dialog.stateDirectoryCopyNotificationMessage',
    defaultMessage: '!!!Directory State Directory copied to clipboard',
    description: 'Message for the wallet address copy success notification.',
  },
});

const COPY_STATE_DIRECTORY_PATH_NOTIFICATION_ID =
  'copy-state-directory-path-notification-id';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class DaedalusDiagnosticsDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  render() {
    const { intl } = this.context;
    const { actions, stores } = this.props;
    const { closeDaedalusDiagnosticsDialog } = actions.app;
    const { restartNode } = actions.networkStatus;
    const { app, networkStatus, profile } = stores;
    const { openExternalLink } = app;
    const { currentLocale } = profile;
    const {
      // Node state
      cardanoNodeState,
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeInSync,
      isNodeTimeCorrect,
      // Application state
      isConnected,
      isSynced,
      syncPercentage,
      hasBeenConnected,
      localTimeDifference,
      isSystemTimeCorrect,
      forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference,
      openStateDirectory,
      getNetworkInfoRequest,
      networkTip,
      localTip,
      localBlockHeight,
      networkBlockHeight,
      latestLocalBlockTimestamp,
      latestNetworkBlockTimestamp,
      isSystemTimeIgnored,
      environment,
      diskSpaceAvailable,
      tlsConfig,
      cardanoNodeID,
      stateDirectoryPath,
    } = networkStatus;

    const systemInfo = {
      platform: environment.os,
      platformVersion: environment.platformVersion,
      cpu: Array.isArray(environment.cpu) ? environment.cpu[0].model : '',
      ram: formattedBytesToSize(environment.ram),
      availableDiskSpace: diskSpaceAvailable,
    };

    const { network, rawNetwork } = environment;
    let cardanoNetwork = networkPrettyNames[network];
    if (rawNetwork && network !== rawNetwork) {
      cardanoNetwork += ` (${capitalize(rawNetwork)})`;
    }

    const coreInfo = {
      daedalusVersion: environment.version,
      daedalusProcessID: environment.rendererProcessID,
      daedalusMainProcessID: environment.mainProcessID,
      isBlankScreenFixActive: environment.isBlankScreenFixActive,
      cardanoVersion: environment.buildNumber,
      cardanoProcessID: cardanoNodeID,
      cardanoAPIPort: tlsConfig ? tlsConfig.port : 0,
      cardanoNetwork,
      daedalusStateDirectoryPath: stateDirectoryPath,
    };

    // Copy-address notification component z-index
    const notificationOrder = 99999;

    return (
      <ReactModal
        isOpen
        closeOnOverlayClick
        onRequestClose={closeDaedalusDiagnosticsDialog.trigger}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <DaedalusDiagnostics
          systemInfo={systemInfo}
          coreInfo={coreInfo}
          cardanoNodeState={cardanoNodeState}
          isDev={environment.isDev}
          isMainnet={environment.isMainnet}
          isStaging={environment.isStaging}
          isTestnet={environment.isTestnet}
          isNodeResponding={isNodeResponding}
          isNodeSubscribed={isNodeSubscribed}
          isNodeSyncing={isNodeSyncing}
          isNodeInSync={isNodeInSync}
          isNodeTimeCorrect={isNodeTimeCorrect}
          isConnected={isConnected}
          isSynced={isSynced}
          syncPercentage={syncPercentage}
          hasBeenConnected={hasBeenConnected}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isForceCheckingNodeTime={forceCheckTimeDifferenceRequest.isExecuting}
          isSystemTimeIgnored={isSystemTimeIgnored}
          latestLocalBlockTimestamp={latestLocalBlockTimestamp}
          latestNetworkBlockTimestamp={latestNetworkBlockTimestamp}
          nodeConnectionError={
            getNetworkInfoRequest.error || forceCheckTimeDifferenceRequest.error
          }
          localTip={localTip}
          networkTip={networkTip}
          localBlockHeight={localBlockHeight}
          networkBlockHeight={networkBlockHeight}
          onForceCheckLocalTimeDifference={forceCheckLocalTimeDifference}
          onOpenStateDirectory={openStateDirectory}
          onOpenExternalLink={openExternalLink}
          onRestartNode={restartNode}
          onClose={closeDaedalusDiagnosticsDialog.trigger}
          onCopyStateDirectoryPath={this.handleCopyStateDirectoryPath}
          currentLocale={currentLocale}
        />
        <GenericNotification
          id={COPY_STATE_DIRECTORY_PATH_NOTIFICATION_ID}
          show={stores.uiNotifications.isOpen(
            COPY_STATE_DIRECTORY_PATH_NOTIFICATION_ID
          )}
          closeNotification={actions.notifications.closeActiveNotification}
          icon="success"
          hasCloseButton
          order={notificationOrder}
          themeOverride="grey"
        >
          {intl.formatMessage(messages.stateDirectoryCopyNotificationMessage)}
        </GenericNotification>
      </ReactModal>
    );
  }

  handleCopyStateDirectoryPath = () => {
    this.props.actions.notifications.open.trigger({
      id: COPY_STATE_DIRECTORY_PATH_NOTIFICATION_ID,
      duration: COPY_STATE_DIRECTORY_PATH_NOTIFICATION_DURATION,
    });
  };
}
