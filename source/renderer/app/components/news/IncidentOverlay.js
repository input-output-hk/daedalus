// @flow
import React, { Component } from 'react';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './IncidentOverlay.scss';

type Props = {
  action?: {
    route?: Function,
    text?: string,
    url?: string,
  },
  content?: {
    h1: string,
    h2?: string,
    paragraph: string,
    bold?: string,
    italic?: string,
  },
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
    const { content } = this.props;
    return (
      showOverlay && (
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={this.onClose}
          />
          <h1 className={styles.h1}>{content.h1}</h1>
          <p className={styles.paragraph}>{content.paragraph}</p>
        </div>
      )
    );
  }
}
