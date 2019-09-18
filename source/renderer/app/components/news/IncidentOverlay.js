// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import ReactMarkdown from 'react-markdown';
import styles from './IncidentOverlay.scss';
import News from '../../domains/News';

type Props = {
  incident: News,
};

@observer
export default class IncidentOverlay extends Component<Props> {
  render() {
    const { content, date, action, title } = this.props.incident;
    return (
      <div className={styles.component}>
        <h1 className={styles.title}>{title}</h1>
        <span className={styles.date}>{moment(date).format('YYYY-MM-DD')}</span>
        <div className={styles.content}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        <button
          className={styles.dismissBtn}
          type="button"
          onClick={action.route}
        >
          {action.label}
        </button>
      </div>
    );
  }
}
