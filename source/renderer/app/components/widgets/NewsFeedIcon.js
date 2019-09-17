// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import styles from './NewsFeedIcon.scss';

type Props = {
  onNewsFeedIconClick: Function,
  newsFeedBellIcon: string,
};

export default class NewsFeedIcon extends Component<Props> {
  render() {
    const { onNewsFeedIconClick, newsFeedBellIcon } = this.props;

    return (
      <button className={styles.component} onClick={onNewsFeedIconClick}>
        <SVGInline className={styles.icon} svg={newsFeedBellIcon} />
      </button>
    );
  }
}
