// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import moment from 'moment';
import News from '../../domains/News';
import styles from './NewsItem.scss';

type Props = {
  newsItem: News,
  onNewsItemActionClick: Function,
};

@observer
export default class NewsItem extends Component<Props> {
  static defaultProps = {
    onNewsItemActionClick: null,
  };

  render() {
    const { onNewsItemActionClick, newsItem } = this.props;
    const actionUrl = newsItem.action.url;
    const componentClasses = classNames([styles.component, newsItem.type]);

    return (
      <div className={componentClasses}>
        <h4 className={styles.newsItemTitle}>{newsItem.title}</h4>
        <div className={styles.newsItemDate}>
          {moment(newsItem.date).format('YYYY-MM-DD')}
        </div>
        <div className={styles.newsItemContentContainer}>
          {newsItem.content}
        </div>
        <button
          className={styles.newsItemActionBtn}
          onClick={event => onNewsItemActionClick(actionUrl, event)}
        >
          {newsItem.action.label}
        </button>
      </div>
    );
  }
}
