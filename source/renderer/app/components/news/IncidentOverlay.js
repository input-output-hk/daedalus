// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import ReactMarkdown from 'react-markdown';
import News from '../../domains/News';
import styles from './IncidentOverlay.scss';

// import type { NewsTypesStateType } from '../../domains/News';

type Props = {
  // incident: NewsTypesStateType
  incident: News.News,
};

@observer
export default class IncidentOverlay extends Component<Props> {
  localizedDateFormat: 'MM/DD/YYYY';

  componentWillMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  renderAction = (action: Object) => {
    if (action && action.url) {
      return <button className={styles.actionBtn}>{action.label}</button>;
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
        <div className={styles.content}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        {this.renderAction(action)}
      </div>
    );
  }
}
