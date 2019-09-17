// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import styles from './NewsFeedIcon.scss';

type Props = {
  onNewsFeedIconClick: Function,
  newsFeedBellIcon: string,
};

export default class NewsFeedIcon extends Component<Props> {
  render() {
    const { onNewsFeedIconClick, newsFeedBellIcon } = this.props;
    const componentClasses = classNames([styles.component]);

    return (
      <button className={componentClasses} onClick={onNewsFeedIconClick}>
        <SVGInline className={styles.icon} svg={newsFeedBellIcon} />
      </button>
    );
  }
}
