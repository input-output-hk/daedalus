// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import IncidentOverlay from '../../components/news/IncidentOverlay';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class IncidentOverlayContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { newsFeedData } = this.props.stores.newsFeed;
    const incident = newsFeedData.incidents;

    return (
      <CenteredLayout>
        {incident && <IncidentOverlay item={incident} />}
      </CenteredLayout>
    );
  }
}
