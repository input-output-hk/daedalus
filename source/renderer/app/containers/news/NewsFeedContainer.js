// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import NewsFeed from '../../components/news/NewsFeed';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class NewsFeedContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleMarkNewsAsRead = (newsTimestamps: number) => {
    const { stores } = this.props;
    const { markNewsAsRead } = stores.newsFeed;
    markNewsAsRead([newsTimestamps]);
  };

  render() {
    const { stores, actions } = this.props;
    const { newsFeedData, isLoadingNews, proceedNewsAction } = stores.newsFeed;
    const { toggleNewsFeed } = actions.app;
    const { openExternalLink, newsFeedIsOpen } = stores.app;

    return (
      <NewsFeed
        news={newsFeedData}
        isNewsFeedOpen={newsFeedIsOpen}
        isLoadingNews={isLoadingNews}
        onClose={toggleNewsFeed.trigger}
        onOpenAlert={stores.newsFeed.openAlert}
        onMarkNewsAsRead={this.handleMarkNewsAsRead}
        openWithoutTransition={stores.networkStatus.environment.isTest}
        onProceedNewsAction={proceedNewsAction}
        onOpenExternalLink={openExternalLink}
      />
    );
  }
}
