// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import ReactMarkdown from 'react-markdown';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import News from '../../domains/News';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import styles from './NewsItem.scss';

type Props = {
  newsItem: News.News,
  onNewsItemActionClick: Function,
  onMarkNewsAsRead: Function,
};

type State = {
  newsItemExpanded: boolean,
};

@observer
export default class NewsItem extends Component<Props, State> {
  static defaultProps = {
    onNewsItemActionClick: null,
  };

  localizedDateFormat: 'MM/DD/YYYY';

  state = {
    newsItemExpanded: false,
  };

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  newsItemClickHandler() {
    if (
      this.props.newsItem.type === 'info' ||
      this.props.newsItem.type === 'announcement'
    ) {
      this.setState(prevState => ({
        newsItemExpanded: !prevState.newsItemExpanded,
      }));
    }
    this.props.onMarkNewsAsRead(this.props.newsItem.date);
  }

  render() {
    const { onNewsItemActionClick, newsItem } = this.props;
    const actionUrl = newsItem.action.url;
    const componentClasses = classNames([
      styles.component,
      newsItem.type,
      this.state.newsItemExpanded ? styles.expanded : null,
    ]);

    return (
      <div
        className={componentClasses}
        role="presentation"
        onClick={this.newsItemClickHandler.bind(this)}
      >
        <h4 className={styles.newsItemTitle}>
          {newsItem.title}
          {!newsItem.read && <span className={styles.newsItemBadge} />}
        </h4>
        <div className={styles.newsItemDate}>
          {moment(newsItem.date).format(this.localizedDateFormat)}
        </div>
        <div className={styles.newsItemContentContainer}>
          <ReactMarkdown escapeHtml={false} source={newsItem.content} />
        </div>
        <button
          className={styles.newsItemActionBtn}
          onClick={event => onNewsItemActionClick(actionUrl, event)}
        >
          {newsItem.action.label}
          <SVGInline svg={externalLinkIcon} />
        </button>
      </div>
    );
  }
}
