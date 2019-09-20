// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import ReactMarkdown from 'react-markdown';
import moment from 'moment';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import AnimateHeight from 'react-animate-height';

import News from '../../domains/News';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import styles from './NewsItem.scss';

type Props = {
  newsItem: News.News,
  onMarkNewsAsRead: Function,
  onOpenExternalLink: Function,
  onOpenAlert?: Function,
  expandWithoutTransition?: boolean,
};

type State = {
  newsItemExpanded: boolean,
  newsItemCollapsible: boolean,
};

@observer
export default class NewsItem extends Component<Props, State> {
  static defaultProps = {
    onNewsItemActionClick: null,
    expandWithoutTransition: false,
  };

  localizedDateFormat: 'MM/DD/YYYY';

  state = {
    newsItemExpanded: false,
    newsItemCollapsible: true,
  };

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  newsItemClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    } else {
      const { type, date } = this.props.newsItem;
      const { newsItemCollapsible } = this.state;
      if (type === 'info' || type === 'announcement') {
        if (newsItemCollapsible) {
          this.setState(prevState => ({
            newsItemExpanded: !prevState.newsItemExpanded,
          }));
        } else {
          this.setState({ newsItemCollapsible: true });
        }
      }
      if (type === 'alert' && this.props.onOpenAlert) {
        this.props.onOpenAlert(date);
      }
      this.props.onMarkNewsAsRead(date);
    }
  }

  newsItemButtonClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    event.preventDefault();
    event.stopPropagation();
    const { onOpenExternalLink, newsItem } = this.props;
    const actionUrl = newsItem.action.url;
    if (actionUrl) {
      onOpenExternalLink(actionUrl, event);
    }
  }

  render() {
    const { newsItem, expandWithoutTransition } = this.props;
    const componentClasses = classNames([
      styles.component,
      newsItem.type ? styles[newsItem.type] : null,
      this.state.newsItemExpanded ? styles.expanded : null,
      newsItem.read ? styles.isRead : null,
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
        <div className={styles.newsItemContentWrapper}>
          <AnimateHeight
            duration={expandWithoutTransition ? 0 : 500}
            height={this.state.newsItemExpanded ? 'auto' : 0}
          >
            <div className={styles.newsItemContentContainer}>
              <ReactMarkdown escapeHtml={false} source={newsItem.content} />
            </div>
            <button
              className={styles.newsItemActionBtn}
              onClick={this.newsItemButtonClickHandler.bind(this)}
            >
              {newsItem.action.label}
              <SVGInline svg={externalLinkIcon} />
            </button>
          </AnimateHeight>
        </div>
      </div>
    );
  }
}
