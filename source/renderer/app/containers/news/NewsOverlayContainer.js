// @flow

// @UPDATE TODO
/* eslint-disable */

import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import AlertsOverlay from '../../components/news/AlertsOverlay';
import IncidentOverlay from '../../components/news/IncidentOverlay';
import UpdateOverlay from '../../components/news/UpdateOverlay';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class NewsOverlayContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { app, newsFeed, profile, appUpdate } = stores;
    const { environment, openExternalLink } = stores.app;
    const { version } = environment;
    const {
      closeOpenedAlert,
      markNewsAsRead,
      newsFeedData,
      openedAlert,
      proceedNewsAction,
      onCloseUpdate,
      isUpdateOpen,
    } = newsFeed;
    const {
      downloadProgress,
      isUpdateDownloaded,
      availableUpdate,
      downloadTimeLeft,
      totalDownloaded,
      totalDownloadSize,
      availableUpdateVersion,
    } = appUpdate;
    const { onInstallUpdate } = actions.appUpdate;
    const { incident, alerts } = newsFeedData;
    const unreadAlerts = alerts.unread;
    const allAlertsCount = alerts.all ? alerts.all.length : 0;
    const { currentDateFormat } = profile;

    const alertToOpen = [];
    if (openedAlert) {
      alertToOpen.push(openedAlert);
    }

    // if (incident)
    //   return (
    //     <IncidentOverlay
    //       incident={incident}
    //       onOpenExternalLink={openExternalLink}
    //       onProceedNewsAction={proceedNewsAction}
    //       currentDateFormat={currentDateFormat}
    //     />
    //   );
    if (unreadAlerts.length > 0)
      return (
        <AlertsOverlay
          alerts={unreadAlerts}
          allAlertsCount={allAlertsCount}
          onCloseOpenAlert={closeOpenedAlert}
          onMarkNewsAsRead={markNewsAsRead}
          onOpenExternalLink={openExternalLink}
          onProceedNewsAction={proceedNewsAction}
          currentDateFormat={currentDateFormat}
        />
      );
    if (alertToOpen.length > 0) {
      return (
        <AlertsOverlay
          alerts={alertToOpen}
          allAlertsCount={allAlertsCount}
          onCloseOpenAlert={closeOpenedAlert}
          onMarkNewsAsRead={markNewsAsRead}
          onOpenExternalLink={openExternalLink}
          onProceedNewsAction={proceedNewsAction}
          currentDateFormat={currentDateFormat}
          hideCounter
        />
      );
    }
    if (availableUpdate && (isUpdateOpen || isUpdateDownloaded)) {
      return (
        <UpdateOverlay
          update={availableUpdate}
          onCloseUpdate={onCloseUpdate}
          downloadProgress={downloadProgress}
          isUpdateDownloaded={isUpdateDownloaded}
          downloadTimeLeft={downloadTimeLeft}
          totalDownloaded={totalDownloaded}
          totalDownloadSize={totalDownloadSize}
          onInstallUpdate={onInstallUpdate.trigger}
          currentVersion={version}
          availableVersion={availableUpdateVersion}
        />
      );
    }
    return null;
  }
}
