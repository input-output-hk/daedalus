import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import { get } from 'lodash';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/close-cros... Remove this comment to see the full error message
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NewsFeed.scss' or its corres... Remove this comment to see the full error message
import styles from './NewsFeed.scss';
import News from '../../domains/News';
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
  onClose: (...args: Array<any>) => any;
  onOpenAlert?: (...args: Array<any>) => any;
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  news?: News.NewsCollection;
  isNewsFeedOpen: boolean;
  onMarkNewsAsRead: (...args: Array<any>) => any;
  openWithoutTransition?: boolean;
  isLoadingNews: boolean;
  currentDateFormat: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  onProceedNewsAction: (...args: Array<any>) => any;
  onOpenAppUpdate: (...args: Array<any>) => any;
  updateDownloadProgress?: number;
  displayAppUpdateNewsItem?: boolean;
  isUpdatePostponed: boolean;
};
type State = {
  hasShadow: boolean;
};
const SCROLLABLE_DOM_ELEMENT_SELECTOR = '.NewsFeed_newsFeedList';

@observer
class NewsFeed extends Component<Props, State> {
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
  scrollableDomElement: HTMLElement | null | undefined = null;
  newsFeedRef = React.createRef<HTMLElement>();
  newsFeedOpenedAt: number;

  componentDidMount() {
    document.addEventListener('click', this.handleWindowClick);
    this.scrollableDomElement = document.querySelector(
      SCROLLABLE_DOM_ELEMENT_SELECTOR
    );
    if (!(this.scrollableDomElement instanceof HTMLElement)) return;
    this.scrollableDomElement.addEventListener('scroll', this.handleOnScroll);
  }

  componentDidUpdate(prevProps: Props) {
    if (!prevProps.isNewsFeedOpen && this.props.isNewsFeedOpen) {
      this.newsFeedOpenedAt = Date.now();
    }
  }

  componentWillUnmount() {
    document.removeEventListener('click', this.handleWindowClick);

    if (this.scrollableDomElement) {
      this.scrollableDomElement.removeEventListener(
        'scroll',
        this.handleOnScroll
      );
    }
  }

  handleWindowClick = (event: MouseEvent) => {
    const newsFeedElement = this.newsFeedRef.current;
    const clickedElement = event.target;
    const { isNewsFeedOpen } = this.props;

    // Detect clicks outside of the newsfeed container
    if (
      isNewsFeedOpen &&
      newsFeedElement &&
      clickedElement instanceof Node &&
      !newsFeedElement.contains(clickedElement)
    ) {
      // This is necessary otherwise the UI click on the newsfeed bell icon
      // would immediately close the newsfeed again
      const msSinceNewsFeedOpened = Date.now() - this.newsFeedOpenedAt;

      if (msSinceNewsFeedOpened > 100) {
        this.props.onClose();
      }
    }
  };
  handleOnScroll = () => {
    const { hasShadow: currentHasShadow } = this.state;

    if (this.scrollableDomElement) {
      const { scrollTop } = this.scrollableDomElement;
      const hasShadow = scrollTop > 0.5;

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
      onOpenAppUpdate,
      updateDownloadProgress = 0,
      isUpdatePostponed,
      displayAppUpdateNewsItem,
    } = this.props;
    const { hasShadow } = this.state;
    const items = get(news, 'all', []);
    const update = get(news, 'update');
    const totalUnreadNewsItems = get(items, 'unread', []).length;
    const hasUpdateItem = displayAppUpdateNewsItem && update;
    const componentClasses = classNames([
      styles.component,
      isNewsFeedOpen ? styles.show : null,
      openWithoutTransition ? styles.noTransition : null,
    ]);
    const newsFeedHeaderStyles = classNames([
      styles.newsFeedHeader,
      hasShadow && !hasUpdateItem ? styles.hasShadow : null,
    ]);
    const newsFeedContainerStyles = classNames([
      styles.newsFeedContainer,
      !hasUpdateItem ? styles.noUpdateItem : null,
      hasShadow ? styles.hasShadow : null,
    ]);
    const newsFeedListStyles = classNames([
      styles.newsFeedList,
      hasUpdateItem ? styles.hasUpdate : null,
      hasShadow ? styles.hasShadow : null,
    ]);
    const newsFeedUpdateStyles = classNames([
      styles.updateItem,
      hasShadow ? styles.hasShadow : null,
    ]);
    return (
      // @ts-ignore ts-migrate(2322) FIXME: Type 'RefObject<HTMLElement>' is not assignable to... Remove this comment to see the full error message
      <div className={componentClasses} ref={this.newsFeedRef}>
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
        <div className={newsFeedContainerStyles}>
          {hasUpdateItem && (
            <div className={newsFeedUpdateStyles}>
              {
                <UpdateItem
                  key={update.id}
                  updateItem={update}
                  // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                  isNewsFeedOpen={isNewsFeedOpen}
                  onMarkNewsAsRead={onMarkNewsAsRead}
                  onOpenAlert={onOpenAlert}
                  onProceedNewsAction={onProceedNewsAction}
                  onOpenAppUpdate={onOpenAppUpdate}
                  currentDateFormat={currentDateFormat}
                  downloadProgress={updateDownloadProgress}
                  isUpdatePostponed={isUpdatePostponed}
                />
              }
            </div>
          )}
          {items.length > 0 && (
            <div className={newsFeedListStyles}>
              {hasUpdateItem && <hr className={styles.separator} />}
              {items.map((newsItem) => (
                <NewsItem
                  key={newsItem.id}
                  hasUpdateItem={hasUpdateItem}
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

export default NewsFeed;
