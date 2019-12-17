// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import ReactMarkdown from 'react-markdown';
import moment from 'moment';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import AnimateHeight from 'react-animate-height';
import News, { NewsTypes } from '../../domains/News';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import styles from './NewsItem.scss';

type Props = {
  newsItem: News.News,
  onMarkNewsAsRead: Function,
  onOpenAlert?: Function,
  onOpenExternalLink: Function,
  onProceedNewsAction: Function,
  expandWithoutTransition?: boolean,
  isNewsFeedOpen: boolean,
  currentDateFormat: string,
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

  newsItemClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    } else {
      const { type, id } = this.props.newsItem;
      const { newsItemCollapsible } = this.state;
      if (type === NewsTypes.INFO || type === NewsTypes.ANNOUNCEMENT) {
        if (newsItemCollapsible) {
          this.setState(prevState => ({
            newsItemExpanded: !prevState.newsItemExpanded,
          }));
        } else {
          this.setState({ newsItemCollapsible: true });
        }
      }
      if (NewsTypes.ALERT && this.props.onOpenAlert) {
        this.props.onOpenAlert(id);
      }
      this.props.onMarkNewsAsRead(id);
    }
  }

  onProceedNewsAction(event: SyntheticMouseEvent<HTMLElement>) {
    const { newsItem, onProceedNewsAction } = this.props;
    onProceedNewsAction(newsItem, event);
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
    const { newsItem, expandWithoutTransition, currentDateFormat } = this.props;
    const componentClasses = classNames([
      styles.component,
      newsItem.type ? styles[newsItem.type] : null,
      this.state.newsItemExpanded ? styles.expanded : null,
      newsItem.read ? styles.isRead : null,
    ]);
    const { url } = newsItem.action;
    const title = this.generateTitleWithBadge(newsItem.title, newsItem.read);

    return (
      <div
        className={componentClasses}
        role="presentation"
        onClick={this.newsItemClickHandler.bind(this)}
      >
        {title}
        <div className={styles.newsItemDate}>
          {moment(newsItem.date).format(currentDateFormat)}
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
                ]}
              />
            </div>
            <button
              className={styles.newsItemActionBtn}
              onClick={this.onProceedNewsAction.bind(this)}
            >
              {newsItem.action.label}
              {url && <SVGInline svg={externalLinkIcon} />}
            </button>
          </AnimateHeight>
        </div>
      </div>
    );
  }
}
