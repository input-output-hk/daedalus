// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import moment from 'moment';
import News /* , { NewsTypes } */ from '../../domains/News';
import styles from './UpdateItem.scss';

type Props = {
  updateItem: News.News,
  onOpenAppUpdate: Function,
  currentDateFormat: string,
  downloadProgress: number,
  isUpdatePostponed: boolean,
};

@observer
export default class UpdateItem extends Component<Props> {
  static defaultProps = {
    onupdateItemActionClick: null,
    expandWithoutTransition: false,
  };

  generateTitleWithBadge = (title: string) => {
    const wordsArray = title.split(' ');
    const lastWordIndex = wordsArray.length - 1;
    const lastWord = wordsArray[lastWordIndex];

    // Remove last word from array
    wordsArray.splice(lastWordIndex, 1);
    // Join words without last one
    const firstSentencePart = wordsArray.join(' ');

    return (
      <h4 className={styles.title}>
        {firstSentencePart ? `${firstSentencePart} ` : null}
        <span className={styles.lastWordWrapper}>
          {lastWord}&nbsp;
          {<span className={styles.badge} />}
        </span>
      </h4>
    );
  };

  render() {
    const {
      updateItem,
      currentDateFormat,
      onOpenAppUpdate,
      downloadProgress,
      isUpdatePostponed,
    } = this.props;
    const componentClasses = classNames([
      styles.component,
      updateItem.type ? styles[updateItem.type] : null,
    ]);
    const title = this.generateTitleWithBadge(updateItem.title);

    return (
      <div
        className={componentClasses}
        role="presentation"
        onClick={onOpenAppUpdate}
      >
        {title}
        <div className={styles.date}>
          {moment(updateItem.date).format(currentDateFormat)}
        </div>
        {!isUpdatePostponed && (
          <div className={styles.downloadProgress}>
            <span style={{ width: `${downloadProgress}%` }} />
            <em />
          </div>
        )}
      </div>
    );
  }
}
