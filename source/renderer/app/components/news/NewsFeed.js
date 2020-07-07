// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { get } from 'lodash';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './NewsFeed.scss';
import News, { NewsTypes } from '../../domains/News';
import NewsItem from './NewsItem';
import UpdateItem from './UpdateItem';
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
  onOpenAlert?: Function,
  news?: News.NewsCollection,
  isNewsFeedOpen: boolean,
  onMarkNewsAsRead: Function,
  openWithoutTransition?: boolean,
  isLoadingNews: boolean,
  currentDateFormat: string,
  onOpenExternalLink: Function,
  onProceedNewsAction: Function,
};

type State = {
  hasShadow: boolean,
};

const SCROLLABLE_DOM_ELEMENT_SELECTOR = '.NewsFeed_newsFeedList';

@observer
export default class NewsFeed extends Component<Props, State> {
  static defaultProps = {
    onClose: null,
    openWithoutTransition: false,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    hasShadow: false,
  };

  scrollableDomElement: ?HTMLElement = null;

  componentDidMount() {
    this.scrollableDomElement = document.querySelector(
      SCROLLABLE_DOM_ELEMENT_SELECTOR
    );
    if (!(this.scrollableDomElement instanceof HTMLElement)) return;
    this.scrollableDomElement.addEventListener('scroll', this.handleOnScroll);
  }

  componentWillUnmount() {
    if (this.scrollableDomElement) {
      this.scrollableDomElement.removeEventListener(
        'scroll',
        this.handleOnScroll
      );
    }
  }

  handleOnScroll = () => {
    const { hasShadow: currentHasShadow } = this.state;

    if (this.scrollableDomElement) {
      const { scrollTop } = this.scrollableDomElement;
      const hasShadow = scrollTop > 3;
      if (currentHasShadow !== hasShadow) {
        this.setState({
          hasShadow,
        });
      }
    }
  };

  render() {
    const { intl } = this.context;
    const {
      news,
      isNewsFeedOpen,
      isLoadingNews,
      onClose,
      onOpenAlert,
      onMarkNewsAsRead,
      openWithoutTransition,
      onProceedNewsAction,
      onOpenExternalLink,
      currentDateFormat,
    } = this.props;
    const { hasShadow } = this.state;

    const items = get(news, 'all', []);
    const newsItems = items.filter(item => item.type !== NewsTypes.UPDATE);
    const updateItems = items.filter(item => item.type === NewsTypes.UPDATE);
    // const items.length = items.length;
    const totalUnreadNewsItems = get(newsItems, 'unread', []).length;
    const componentClasses = classNames([
      styles.component,
      isNewsFeedOpen ? styles.show : null,
      openWithoutTransition ? styles.noTransition : null,
    ]);

    const newsFeedHeaderStyles = classNames([
      styles.newsFeedHeader,
      hasShadow ? styles.hasShadow : null,
    ]);

    return (
      <div className={componentClasses}>
        <div className={newsFeedHeaderStyles}>
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
          {updateItems.length && (
            <div className={styles.newsFeedItemsContainer}>
              {updateItems.map(updateItem => (
                <UpdateItem
                  key={updateItem.id}
                  updateItem={updateItem}
                  isNewsFeedOpen={isNewsFeedOpen}
                  onMarkNewsAsRead={onMarkNewsAsRead}
                  onOpenAlert={onOpenAlert}
                  onProceedNewsAction={onProceedNewsAction}
                  onOpenExternalLink={onOpenExternalLink}
                  currentDateFormat={currentDateFormat}
                  downloadProgress={50}
                />
              ))}
              <hr className={styles.separator} />
            </div>
          )}
          {newsItems.length && (
            <div className={styles.newsFeedItemsContainer}>
              {newsItems.map(newsItem => (
                <NewsItem
                  key={newsItem.id}
                  newsItem={newsItem}
                  isNewsFeedOpen={isNewsFeedOpen}
                  onMarkNewsAsRead={onMarkNewsAsRead}
                  onOpenAlert={onOpenAlert}
                  onProceedNewsAction={onProceedNewsAction}
                  onOpenExternalLink={onOpenExternalLink}
                  currentDateFormat={currentDateFormat}
                />
              ))}
            </div>
          )}
          {news && items.length === 0 && !isLoadingNews && (
            <div className={styles.newsFeedEmptyContainer}>
              <p className={styles.newsFeedEmpty}>
                {intl.formatMessage(messages.newsFeedEmpty)}
              </p>
            </div>
          )}
          {(!news || items.length === 0) && isLoadingNews && (
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
