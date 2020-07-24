// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import AppUpdateOverlay from '../../components/appUpdate/AppUpdateOverlay';
import AppManualUpdateOverlay from '../../components/appUpdate/AppManualUpdateOverlay';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class AppUpdateContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { appUpdate } = stores;
    const { environment, openExternalLink } = stores.app;
    const { version } = environment;
    const {
      downloadProgress,
      isUpdateDownloaded,
      availableUpdate,
      downloadTimeLeft,
      totalDownloaded,
      totalDownloadSize,
      availableUpdateVersion,
      isAutomaticUpdateFailed,
    } = appUpdate;
    const { installUpdate, closeAppUpdateOverlay } = actions.appUpdate;

    if (isAutomaticUpdateFailed) {
      return (
        <AppManualUpdateOverlay
          currentAppVersion={version}
          availableAppVersion={availableUpdateVersion}
          onExternalLinkClick={openExternalLink}
        />
      );
    }

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
      />
    );
  }
}
