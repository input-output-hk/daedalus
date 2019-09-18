// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import attentionIcon from '../../assets/images/attention-big-light.inline.svg';
import styles from './AlertsOverlay.scss';
import News from '../../domains/News';

type State = {
  showOverlay: boolean,
};

type Props = {
  alerts: Array<News>,
};

@observer
export default class AlertsOverlay extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      showOverlay: true,
    };
  }

  onClose = () => this.setState({ showOverlay: false });

  renderAction = (action: Object) => {
    if (action && action.url) {
      return <a href={action.url}>{action.label}</a>;
    }
  };

  render() {
    const { showOverlay } = this.state;
    const { alerts } = this.props;
    const alert = alerts[0];
    const { content, date, action, title } = alert;
    return (
      showOverlay && (
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={this.onClose}
          />
          <SVGInline svg={attentionIcon} className={styles.icon} />
          <h1 className={styles.title}>{title}</h1>
          <span className={styles.date}>
            {moment(date).format('YYYY-MM-DD')}
          </span>
          <p className={styles.content}>{content}</p>
          {this.renderAction(action)}
        </div>
      )
    );
  }
}
