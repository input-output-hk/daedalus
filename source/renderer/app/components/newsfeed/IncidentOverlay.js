// @flow
import React, { Component } from 'react';
import moment from 'moment';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './IncidentOverlay.scss';

type Props = {
  action?: {
    route?: Function,
    text?: string,
    url?: string,
  },
  content: {
    h1: string,
    h2: string,
    paragraph: string,
    bold: string,
    italic: string,
  },
  date: Date,
  target?: {
    daedalus_version?: string,
    target_os?: string,
    target_os_version?: string,
  },
  title?: string,
  type?: string,
};

type State = {
  showOverlay: boolean,
};

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
    const { content, date, title } = this.props;
    return (
      showOverlay && (
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={this.onClose}
          />
          <h1 className={styles.title}>{title}</h1>
          <h1 className={styles.h1}>{content.h1}</h1>
          <span className={styles.date}>
            {moment(date).format('YYYY-MM-DD')}
          </span>
          <h2 className={styles.h2}>{content.h2}</h2>
          <p className={styles.paragraph}>{content.paragraph}</p>
          <bold className={styles.bold}>{content.bold}</bold>
          <i className={styles.italic}>{content.italic}</i>
        </div>
      )
    );
  }
}
