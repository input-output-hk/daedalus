// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import ReactMarkdown from 'react-markdown';
import DialogCloseButton from '../widgets/DialogCloseButton';
import styles from './AlertsOverlay.scss';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import News from '../../domains/News';

type State = {
  showOverlay: boolean,
};

type Props = {
  alerts: Array<News.News>,
  onMarkNewsAsRead: Function,
};

@observer
export default class AlertsOverlay extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      showOverlay: true,
    };
  }

  onClose = () => {
    const { alerts } = this.props;
    if (alerts.length <= 1) {
      this.props.onMarkNewsAsRead(alerts[0].date);
      this.setState({ showOverlay: false });
      return;
    }
    this.props.onMarkNewsAsRead(alerts[0].date);
  };

  renderAction = (action: Object) => {
    if (action && action.url) {
      return <button className={styles.actionBtn}>{action.label}</button>;
    }
    return null;
  };

  renderCounter = (alerts: Array<News.News>) => {
    if (alerts.length > 1) {
      return <span className={styles.counter}>1 / {alerts.length}</span>;
    }
    return null;
  };

  render() {
    const { showOverlay } = this.state;
    const { alerts } = this.props;
    const [alert] = alerts;
    const { content, date, action, title } = alert;
    return (
      showOverlay && (
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={this.onClose}
          />
          {this.renderCounter(alerts)}
          <h1 className={styles.title}>{title}</h1>
          <span className={styles.date}>
            {moment(date).format('YYYY-MM-DD')}
          </span>
          <div className={styles.content}>
            <ReactMarkdown escapeHtml={false} source={content} />
          </div>
          {this.renderAction(action)}
        </div>
      )
    );
  }
}
