// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import AppUpdateOverlay from '../../components/appUpdate/AppUpdateOverlay';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class AppUpdateContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { appUpdate } = stores;
    const { environment } = stores.app;
    const { version } = environment;
    const {
      downloadProgress,
      isUpdateDownloaded,
      availableUpdate,
      downloadTimeLeft,
      totalDownloaded,
      totalDownloadSize,
      availableUpdateVersion,
    } = appUpdate;
    const { installUpdate, closeAppUpdateOverlay } = actions.appUpdate;

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
        currentVersion={version}
        availableVersion={availableUpdateVersion}
      />
    );
  }
}
