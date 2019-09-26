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
  onGoToRoute: Function,
  expandWithoutTransition?: boolean,
  isNewsFeedOpen: boolean,
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

  componentWillReceiveProps(nextProps: Props) {
    const { newsItemExpanded } = this.state;
    if (
      this.props.isNewsFeedOpen &&
      !nextProps.isNewsFeedOpen &&
      newsItemExpanded
    ) {
      this.setState({ newsItemExpanded: false });
    }
  }

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
    const { onOpenExternalLink, newsItem, onGoToRoute } = this.props;
    const { url, route } = newsItem.action;

    if (url) {
      onOpenExternalLink(url, event);
    } else if (route) {
      onGoToRoute(route);
    }
  }

  generateTitleWithBadge = (title: string, isRead: boolean) => {
    const wordsArray = title.split(' ');
    const lastWordIndex = wordsArray.length - 1;
    const lastWord = wordsArray[lastWordIndex];

    // Remove last word from array
    wordsArray.splice(lastWordIndex, 1);
    // Join words without last one
    const firstSentencePart = wordsArray.join(' ');

    return (
      <h4 className={styles.newsItemTitle}>
        {firstSentencePart ? `${firstSentencePart} ` : null}
        <span className={styles.lastWordWrapper}>
          {lastWord}&nbsp;
          {!isRead && <span className={styles.newsItemBadge} />}
        </span>
      </h4>
    );
  };

  render() {
    const { newsItem, expandWithoutTransition } = this.props;
    const componentClasses = classNames([
      styles.component,
      newsItem.type ? styles[newsItem.type] : null,
      this.state.newsItemExpanded ? styles.expanded : null,
      newsItem.read ? styles.isRead : null,
    ]);
    const { route } = newsItem.action;
    const title = this.generateTitleWithBadge(newsItem.title, newsItem.read);

    return (
      <div
        className={componentClasses}
        role="presentation"
        onClick={this.newsItemClickHandler.bind(this)}
      >
        {title}
        <div className={styles.newsItemDate}>
          {moment(newsItem.date).format(this.localizedDateFormat)}
        </div>
        <div className={styles.newsItemContentWrapper}>
          <AnimateHeight
            duration={expandWithoutTransition ? 0 : 500}
            height={this.state.newsItemExpanded ? 'auto' : 0}
          >
            <div className={styles.newsItemContentContainer}>
              <ReactMarkdown
                escapeHtml={false}
                source={newsItem.content}
                disallowedTypes={[
                  'image',
                  'imageReference',
                  'table',
                  'definition',
                  'inlineCode',
                  'code',
                  'html',
                  'virtualHtml',
                  'parsedHtml',
                ]}
              />
            </div>
            <button
              className={styles.newsItemActionBtn}
              onClick={this.newsItemButtonClickHandler.bind(this)}
            >
              {newsItem.action.label}
              {!route && <SVGInline svg={externalLinkIcon} />}
            </button>
          </AnimateHeight>
        </div>
      </div>
    );
  }
}
