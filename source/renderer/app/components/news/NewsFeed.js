// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './NewsFeed.scss';
import News from '../../domains/News';

const messages = defineMessages({
  newsFeedEmpty: {
    id: 'news.newsfeed.empty',
    defaultMessage: 'News feed is empty',
    description: 'News feed is empty',
  },
  newsFeedNoFetch: {
    id: 'news.newsfeed.noFetch',
    defaultMessage: 'News feed couldn’t be fetched',
    description: 'News feed couldn’t be fetched',
  },
  newsFeedReload: {
    id: 'news.newsfeed.reload',
    defaultMessage: 'Reload',
    description: 'Reload',
  },
  newsFeedTitle: {
    id: 'news.newsfeed.title',
    defaultMessage: 'News feed',
    description: 'News feed',
  },
});

type Props = {
  onClose: Function,
  news: Array<News>,
  newsFeedShowClass: string,
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
    const { onClose, news, newsFeedShowClass } = this.props;
    const totalNewsItems = get(this.props, 'news').length;

    const componentClasses = classNames([
      styles.component,
      newsFeedShowClass ? styles.show : null,
    ]);
    return (
      <div className={componentClasses}>
        <div className={styles.newsFeedHeader}>
          <h3 className={styles.newsFeedTitle}>
            {intl.formatMessage(messages.newsFeedTitle)}
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
              <button className={styles.newsFeedReloadBtn}>
                {intl.formatMessage(messages.newsFeedReload)}
              </button>
            </div>
          )}
          {news && totalNewsItems === 0 && (
            <div className={styles.newsFeedNoFetchContainer}>
              <p className={styles.newsFeedNoFetch}>
                {intl.formatMessage(messages.newsFeedEmpty)}
              </p>
            </div>
          )}
          {news && totalNewsItems > 0 && (
            <div className={styles.newsFeedNoFetchContainer}>News items</div>
          )}
        </div>
      </div>
    );
  }
}
