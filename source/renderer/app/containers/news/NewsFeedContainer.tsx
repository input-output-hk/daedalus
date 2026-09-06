import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import NewsFeed from '../../components/news/NewsFeed';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class NewsFeedContainer extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleMarkNewsAsRead = (newsId: number) => {
    const { stores } = this.props;
    const { markNewsAsRead } = stores.newsFeed;
    markNewsAsRead([newsId]);
  };

  render() {
    const { stores, actions } = this.props;
    const { app, profile, appUpdate, newsFeed } = stores;
    const { newsFeedData, isLoadingNews, proceedNewsAction } = newsFeed;
    const { openAppUpdateOverlay } = actions.appUpdate;
    const {
      downloadProgress,
      displayAppUpdateNewsItem,
      isUpdatePostponed,
    } = appUpdate;
    const { toggleNewsFeed } = actions.app;
    const { openExternalLink, newsFeedIsOpen } = app;
    const { currentDateFormat } = profile;
    return (
      <NewsFeed
        news={newsFeedData}
        isNewsFeedOpen={newsFeedIsOpen}
        isLoadingNews={isLoadingNews}
        onClose={toggleNewsFeed.trigger}
        onOpenAlert={newsFeed.openAlert}
        onMarkNewsAsRead={this.handleMarkNewsAsRead}
        openWithoutTransition={stores.networkStatus.environment.isTest}
        onProceedNewsAction={proceedNewsAction}
        onOpenExternalLink={openExternalLink}
        currentDateFormat={currentDateFormat}
        updateDownloadProgress={downloadProgress}
        displayAppUpdateNewsItem={displayAppUpdateNewsItem}
        isUpdatePostponed={isUpdatePostponed}
        onOpenAppUpdate={openAppUpdateOverlay.trigger}
      />
    );
  }
}

export default NewsFeedContainer;
