// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import ReactMarkdown from 'react-markdown';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
import styles from './AlertsOverlay.scss';

type State = {
  showOverlay: boolean,
};

type Props = {
  alerts: Array<{}>,
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
    if (this.props.alerts.length <= 1) {
      this.props.onMarkNewsAsRead(this.props.alerts[0].id);
      this.setState({ showOverlay: false });
      return;
    }
    this.props.onMarkNewsAsRead(this.props.alerts[0].id);
  };

  renderAction = (action: Object) => {
    if (action && action.url) {
      return <button className={styles.actionBtn}>{action.label}</button>;
    }
    return null;
  };

  renderCounter = (alerts: Array<{}>) => {
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
          <SVGInline svg={attentionIcon} className={styles.icon} />
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
