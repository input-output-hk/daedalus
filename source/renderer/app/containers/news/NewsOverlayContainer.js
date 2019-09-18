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
    const { incident, alerts } = this.props.stores.newsFeed;
    if (incident) return <IncidentOverlay incident={incident} />;
    if (alerts.length) return <AlertsOverlay alerts={alerts} />;
    return null;
  }
}
