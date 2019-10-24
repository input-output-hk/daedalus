// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import ReactMarkdown from 'react-markdown';
import SVGInline from 'react-svg-inline';
import News from '../../domains/News';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import styles from './IncidentOverlay.scss';

type Props = {
  incident: News.News,
  onOpenExternalLink: Function,
  onProceedNewsAction: Function,
};

@observer
export default class IncidentOverlay extends Component<Props> {
  localizedDateFormat: 'MM/DD/YYYY';

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  contentClickHandler(event: SyntheticMouseEvent<HTMLElement>) {
    const linkUrl = get(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      this.props.onOpenExternalLink(linkUrl);
    }
  }

  onProceedNewsAction(event: SyntheticMouseEvent<HTMLElement>) {
    const { incident, onProceedNewsAction } = this.props;
    onProceedNewsAction(incident, event);
  }

  renderAction = (action: Object) => {
    if (action && (action.url || action.event)) {
      return (
        <button
          className={styles.actionBtn}
          onClick={this.onProceedNewsAction.bind(this)}
        >
          {action.label}
          {action.url && <SVGInline svg={externalLinkIcon} />}
        </button>
      );
    }
    return null;
  };

  render() {
    const { incident } = this.props;
    const { content, date, action, title } = incident;
    return (
      <div className={styles.component}>
        <h1 className={styles.title}>{title}</h1>
        <span className={styles.date}>
          {moment(date).format(this.localizedDateFormat)}
        </span>
        <div
          className={styles.content}
          role="presentation"
          onClick={this.contentClickHandler.bind(this)}
        >
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {this.renderAction(action)}
      </div>
    );
  }
}
