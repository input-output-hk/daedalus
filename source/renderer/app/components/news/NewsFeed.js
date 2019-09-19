// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { get } from 'lodash';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './NewsFeed.scss';
import News from '../../domains/News';
import NewsItem from './NewsItem';
import LoadingSpinner from '../widgets/LoadingSpinner';

const messages = defineMessages({
  newsFeedEmpty: {
    id: 'news.newsfeed.empty',
    defaultMessage: 'Newsfeed is empty',
    description: 'Newsfeed is empty',
  },
  newsFeedNoFetch: {
    id: 'news.newsfeed.noFetch',
    defaultMessage: 'Trying to fetch the newsfeed...',
    description: 'Trying to fetch the newsfeed...',
  },
  newsFeedTitle: {
    id: 'news.newsfeed.title',
    defaultMessage: 'Newsfeed',
    description: 'Newsfeed',
  },
});

type Props = {
  onClose: Function,
  onNewsItemActionClick: Function,
  onOpenAlert: Function,
  news?: News.NewsCollection,
  newsFeedShowClass: boolean,
  onMarkNewsAsRead: Function,
  openWithoutTransition?: boolean,
  isLoadingNews: boolean,
};

@observer
export default class NewsFeed extends Component<Props> {
  static defaultProps = {
    onClose: null,
    openWithoutTransition: false,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onNewsItemActionClick,
      onOpenAlert,
      news,
      newsFeedShowClass,
      onMarkNewsAsRead,
      openWithoutTransition,
      isLoadingNews,
    } = this.props;

    const totalNewsItems = get(news, 'all', 0).length;
    const totalUnreadNewsItems = get(news, 'unread', 0).length;
    const componentClasses = classNames([
      styles.component,
      newsFeedShowClass ? styles.show : null,
      openWithoutTransition ? styles.noTransition : null,
    ]);

    return (
      <div className={componentClasses}>
        <div className={styles.newsFeedHeader}>
          <h3 className={styles.newsFeedTitle}>
            {intl.formatMessage(messages.newsFeedTitle)}
            {totalUnreadNewsItems > 0 && (
              <span className={styles.newsFeedBadge}>
                {totalUnreadNewsItems}
              </span>
            )}
          </h3>
          <button onClick={onClose} className={styles.newsFeedCloseBtn}>
            <SVGInline svg={closeCrossThin} />
          </button>
        </div>
        <div className={styles.newsFeedList}>
          {news && totalNewsItems > 0 && (
            <div className={styles.newsFeedItemsContainer}>
              {news.all.map(newsItem => (
                <NewsItem
                  key={newsItem.date}
                  onNewsItemActionClick={onNewsItemActionClick}
                  onOpenAlert={onOpenAlert}
                  newsItem={newsItem}
                  onMarkNewsAsRead={onMarkNewsAsRead}
                />
              ))}
            </div>
          )}
          {news && totalNewsItems === 0 && !isLoadingNews && (
            <div className={styles.newsFeedEmptyContainer}>
              <p className={styles.newsFeedEmpty}>
                {intl.formatMessage(messages.newsFeedEmpty)}
              </p>
            </div>
          )}
          {(!news || totalNewsItems === 0) && isLoadingNews && (
            <div className={styles.newsFeedNoFetchContainer}>
              <p className={styles.newsFeedNoFetch}>
                {intl.formatMessage(messages.newsFeedNoFetch)}
              </p>
              <LoadingSpinner medium />
            </div>
          )}
        </div>
      </div>
    );
  }
}
