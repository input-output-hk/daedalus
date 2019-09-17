// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './NewsFeed.scss';

const messages = defineMessages({
  newsFeedTitle: {
    id: 'news.newsfeed.title',
    defaultMessage: 'News feed',
    description: 'News feed',
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
  }
});

type Props = {
  onClose: Function,
  news?: []
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
    const { onClose, news } = this.props;
    const totalNewsItems = get(news, 'items').length;
    return (
      <div className={styles.component}>
        <div className={styles.newsFeedHeader}>
          <h3 className={styles.newsFeedTitle}>
            {intl.formatMessage(messages.newsFeedTitle)}
          </h3>
          <button
            onClick={onClose}
            className={styles.newsFeedCloseBtn}
          >
            <SVGInline svg={closeCrossThin} />
          </button>
        </div>
        <div className={styles.newsFeedList}>
          {!totalNewsItems && (
            <div className={styles.newsFeedNoFetchContainer}>
              <p className={styles.newsFeedNoFetch}>
                {intl.formatMessage(messages.newsFeedNoFetch)}
              </p>
              <button className={styles.newsFeedReloadBtn}>
                {intl.formatMessage(messages.newsFeedReload)}
              </button>
            </div>
          )}
        </div>
      </div>
    );
  }
}
