// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import NewsFeed from '../../components/news/NewsFeed';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class IncidentOverlayContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    return <NewsFeed />;
  }
}
