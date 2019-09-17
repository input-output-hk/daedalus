// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import DialogCloseButton from '../widgets/DialogCloseButton';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import styles from './IncidentOverlay.scss';
import type { NewsItem } from '../../api/news/types';

type State = {
  showOverlay: boolean,
};

@observer
export default class IncidentOverlay extends Component<NewsItem, State> {
  constructor(props: NewsItem) {
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
          <h1 className={styles.title}>{title[0]}</h1>
          <span className={styles.date}>
            {moment(date).format('YYYY-MM-DD')}
          </span>
          <p className={styles.p}>{content[0]}</p>
          {/* <p className={styles.paragraph}>{content.paragraph}</p>
          <bold className={styles.bold}>{content.bold}</bold>
          <i className={styles.italic}>{content.italic}</i> */}
        </div>
      )
    );
  }
}
