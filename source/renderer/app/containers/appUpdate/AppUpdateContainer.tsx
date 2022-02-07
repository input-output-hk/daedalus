import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import AppUpdateOverlay from '../../components/appUpdate/AppUpdateOverlay';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class AppUpdateContainer extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { stores, actions } = this.props;
    const { appUpdate } = stores;
    const { environment, openExternalLink } = stores.app;
    const { version, isLinux, isTestnet } = environment;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
    const { isFlight } = global;
    const {
      downloadProgress,
      isUpdateDownloaded,
      availableUpdate,
      downloadTimeLeft,
      totalDownloaded,
      totalDownloadSize,
      availableUpdateVersion,
      isAutomaticUpdateFailed,
      isWaitingToQuitDaedalus,
      installationProgress,
    } = appUpdate;
    const {
      installUpdate,
      closeAppUpdateOverlay,
      postponeUpdate,
    } = actions.appUpdate;
    if (!availableUpdate) return null;
    return (
      <AppUpdateOverlay
        update={availableUpdate}
        onClose={closeAppUpdateOverlay.trigger}
        downloadProgress={downloadProgress}
        isUpdateDownloaded={isUpdateDownloaded}
        downloadTimeLeft={downloadTimeLeft}
        totalDownloaded={totalDownloaded}
        totalDownloadSize={totalDownloadSize}
        onInstallUpdate={installUpdate.trigger}
        currentAppVersion={version}
        availableAppVersion={availableUpdateVersion}
        isAutomaticUpdateFailed={isAutomaticUpdateFailed}
        onExternalLinkClick={openExternalLink}
        onPostponeUpdate={postponeUpdate.trigger}
        isWaitingToQuitDaedalus={isWaitingToQuitDaedalus}
        installationProgress={installationProgress}
        isLinux={isLinux}
        isFlight={isFlight}
        isTestnet={isTestnet}
      />
    );
  }
}

export default AppUpdateContainer;
