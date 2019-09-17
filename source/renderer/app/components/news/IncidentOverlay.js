// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import styles from './IncidentOverlay.scss';
import News from '../../domains/News';

type State = {
  showOverlay: boolean,
};

type Props = {
  incident: News,
};

@observer
export default class IncidentOverlay extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      showOverlay: true,
    };
  }

  onClose = () => this.setState({ showOverlay: false });

  render() {
    const { showOverlay } = this.state;
    const { content, date, action, title } = this.props.incident;
    return (
      showOverlay && (
        <div className={styles.component}>
          <h1 className={styles.title}>{title}</h1>
          <span className={styles.date}>
            {moment(date).format('YYYY-MM-DD')}
          </span>
          <p className={styles.content}>{content}</p>
          <button
            className={styles.dismissBtn}
            type="button"
            onClick={this.onClose}
          >
            {action.label}
          </button>
        </div>
      )
    );
  }
}
