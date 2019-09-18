// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './NewsFeed.scss';
import News from '../../domains/News';
import NewsItem from './NewsItem';

const messages = defineMessages({
  newsFeedEmpty: {
    id: 'news.newsfeed.empty',
    defaultMessage: 'News feed is empty',
    description: 'News feed is empty',
  },
  newsFeedNoFetch: {
    id: 'news.newsfeed.noFetch',
    defaultMessage: 'Trying to fetch the news feed...',
    description: 'Trying to fetch the news feed...',
  },
  newsFeedTitle: {
    id: 'news.newsfeed.title',
    defaultMessage: 'News feed',
    description: 'News feed',
  },
});

type Props = {
  onClose: Function,
  onNewsItemActionClick: Function,
  news?: News.NewsCollection,
  newsFeedShowClass: boolean,
  onMarkNewsAsRead: Function,
};

@observer
export default class NewsFeed extends Component<Props> {
  static defaultProps = {
    onClose: null,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      onNewsItemActionClick,
      news,
      newsFeedShowClass,
      onMarkNewsAsRead,
    } = this.props;

    const totalNewsItems = news && news.all ? news.all.length : 0;
    const componentClasses = classNames([
      styles.component,
      newsFeedShowClass ? styles.show : null,
    ]);

    return (
      <div className={componentClasses}>
        <div className={styles.newsFeedHeader}>
          <h3 className={styles.newsFeedTitle}>
            {intl.formatMessage(messages.newsFeedTitle)}
            {news.unread && news.unread.length > 0 && (
              <span className={styles.newsFeedBadge}>{news.unread.length}</span>
            )}
          </h3>
          <button onClick={onClose} className={styles.newsFeedCloseBtn}>
            <SVGInline svg={closeCrossThin} />
          </button>
        </div>
        <div className={styles.newsFeedList}>
          {!news && (
            <div className={styles.newsFeedNoFetchContainer}>
              <p className={styles.newsFeedNoFetch}>
                {intl.formatMessage(messages.newsFeedNoFetch)}
              </p>
            </div>
          )}
          {news && totalNewsItems === 0 && (
            <div className={styles.newsFeedEmptyContainer}>
              <p className={styles.newsFeedEmpty}>
                {intl.formatMessage(messages.newsFeedEmpty)}
              </p>
            </div>
          )}
          {news && totalNewsItems > 0 && (
            <div className={styles.newsFeedItemsContainer}>
              {news.all.map((newsItem, index) => (
                <NewsItem
                  // eslint-disable-next-line react/no-array-index-key
                  key={index}
                  onNewsItemActionClick={onNewsItemActionClick}
                  newsItem={newsItem}
                  onMarkNewsAsRead={onMarkNewsAsRead}
                />
              ))}
              <div />
            </div>
          )}
        </div>
      </div>
    );
  }
}
