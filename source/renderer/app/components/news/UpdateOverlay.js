// @flow
import React, { Component } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import ReactMarkdown from 'react-markdown';
import News from '../../domains/News';
import styles from './UpdateOverlay.scss';
import DialogCloseButton from '../widgets/DialogCloseButton';
import ProgressBarLarge from '../widgets/ProgressBarLarge';

type Props = {
  update: News.News,
  currentDateFormat: string,
  onCloseUpdate: Function,
  downloadProgress?: number,
};

@observer
export default class UpdateOverlay extends Component<Props> {
  localizedDateFormat: 'MM/DD/YYYY';

  componentDidMount() {
    this.localizedDateFormat = moment.localeData().longDateFormat('L');
  }

  render() {
    const {
      update,
      currentDateFormat,
      onCloseUpdate,
      downloadProgress,
    } = this.props;
    const { content, date, title } = update;
    return (
      <div
        className={styles.component}
        role="presentation"
        onClick={onCloseUpdate}
      >
        <DialogCloseButton onClose={onCloseUpdate} />
        <h1 className={styles.title}>{title}</h1>
        <span className={styles.date}>
          {moment(date).format(currentDateFormat)}
        </span>
        <div className={styles.content}>
          <ReactMarkdown escapeHtml={false} source={content} />
        </div>
        <div className={styles.downloadProgress}>
          <ProgressBarLarge progress={downloadProgress} />
        </div>
      </div>
    );
  }
}
