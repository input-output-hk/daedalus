// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import NewsFeed from '../../components/news/NewsFeed';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class NewsFeedContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { newsFeedData } = this.props.stores.newsFeed;
    const { toggleNewsFeed } = this.props.actions.app;
    const newsFeedShowClass = this.props.stores.app.newsFeedIsOpen;
    return (
      <NewsFeed
        news={newsFeedData}
        onClose={toggleNewsFeed.trigger}
        newsFeedShowClass={newsFeedShowClass}
      />
    );
  }
}
