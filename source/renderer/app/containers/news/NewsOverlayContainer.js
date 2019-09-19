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
    const {
      markNewsAsRead,
      newsFeedData,
      openedAlert,
    } = this.props.stores.newsFeed;
    const { incident, alerts } = newsFeedData;
    const unreadAlerts = alerts.unread;
    const alertToOpen = [];
    if (openedAlert) {
      alertToOpen.push(openedAlert);
    }
    if (incident) return <IncidentOverlay incident={incident} />;
    if (unreadAlerts.length > 0)
      return (
        <AlertsOverlay
          onMarkNewsAsRead={markNewsAsRead}
          alerts={unreadAlerts}
        />
      );
    if (alertToOpen.length > 0) {
      return (
        <AlertsOverlay onMarkNewsAsRead={markNewsAsRead} alerts={alertToOpen} />
      );
    }
    return null;
  }
}
