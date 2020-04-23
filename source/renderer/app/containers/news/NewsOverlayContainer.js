// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import AlertsOverlay from '../../components/news/AlertsOverlay';
import IncidentOverlay from '../../components/news/IncidentOverlay';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class NewsOverlayContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { app, newsFeed, profile } = this.props.stores;
    const { openExternalLink } = app;
    const {
      closeOpenedAlert,
      markNewsAsRead,
      newsFeedData,
      openedAlert,
      proceedNewsAction,
    } = newsFeed;
    const { incident, alerts } = newsFeedData;
    const unreadAlerts = alerts.unread;
    const allAlertsCount = alerts.all ? alerts.all.length : 0;
    const { currentDateFormat } = profile;

    const alertToOpen = [];
    if (openedAlert) {
      alertToOpen.push(openedAlert);
    }

    if (incident)
      return (
        <IncidentOverlay
          incident={incident}
          onOpenExternalLink={openExternalLink}
          onProceedNewsAction={proceedNewsAction}
          currentDateFormat={currentDateFormat}
        />
      );
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
    return null;
  }
}
