import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import ReactMarkdown from 'react-markdown';
import moment from 'moment';
import { get } from 'lodash';
import AnimateHeight from 'react-animate-height';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import News, { NewsTypes } from '../../domains/News';
import ButtonLink from '../widgets/ButtonLink';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NewsItem.scss' or its corres... Remove this comment to see the full error message
import styles from './NewsItem.scss';

type Props = {
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  newsItem: News.News;
  onMarkNewsAsRead: (...args: Array<any>) => any;
  onOpenAlert?: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  onProceedNewsAction: (...args: Array<any>) => any;
  expandWithoutTransition?: boolean;
  isNewsFeedOpen: boolean;
  currentDateFormat: string;
  hasUpdateItem?: boolean;
};
type State = {
  newsItemExpanded: boolean;
  newsItemCollapsible: boolean;
};

@observer
class NewsItem extends Component<Props, State> {
  static defaultProps = {
    onNewsItemActionClick: null,
    expandWithoutTransition: false,
  };
  state = {
    newsItemExpanded: false,
    newsItemCollapsible: true,
  };

  componentDidUpdate(prevProps: Props) {
    const { newsItemExpanded } = this.state;

    if (
      prevProps.isNewsFeedOpen &&
      !this.props.isNewsFeedOpen &&
      newsItemExpanded
    ) {
      this.setState({
        newsItemExpanded: false,
      }); // eslint-disable-line
    }
  }

  newsItemClickHandler(event: React.MouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);

    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    } else {
      const { type, id } = this.props.newsItem;
      const { newsItemCollapsible } = this.state;

      if (type === NewsTypes.INFO || type === NewsTypes.ANNOUNCEMENT) {
        if (newsItemCollapsible) {
          this.setState((prevState) => ({
            newsItemExpanded: !prevState.newsItemExpanded,
          }));
        } else {
          this.setState({
            newsItemCollapsible: true,
          });
        }
      }

      if (NewsTypes.ALERT && this.props.onOpenAlert) {
        this.props.onOpenAlert(id);
      }

      this.props.onMarkNewsAsRead(id);
    }
  }

  onProceedNewsAction(event: React.MouseEvent<HTMLElement>) {
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
    const {
      newsItem,
      expandWithoutTransition,
      currentDateFormat,
      hasUpdateItem,
    } = this.props;
    const componentClasses = classNames([
      styles.component,
      newsItem.type ? styles[newsItem.type] : null,
      this.state.newsItemExpanded ? styles.expanded : null,
      newsItem.read ? styles.isRead : null,
      !hasUpdateItem ? styles.noUpdateItem : null,
    ]);
    const { url = '' } = newsItem.action;
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
            <ButtonLink
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className={styles.newsItemActionBtn}
              onClick={(e) => this.onProceedNewsAction(e)}
              skin={ButtonSkin}
              label={newsItem.action.label}
              linkProps={{
                className: styles.externalLink,
                hasIconBefore: false,
                hasIconAfter: url.length > 0,
              }}
            />
          </AnimateHeight>
        </div>
      </div>
    );
  }
}

export default NewsItem;
