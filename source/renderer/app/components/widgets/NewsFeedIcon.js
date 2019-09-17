// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import newsFeedBellIcon from '../../assets/images/top-bar/bell.inline.svg';
import styles from './NewsFeedIcon.scss';

type Props = {
  onNewsFeedIconClick: Function,
  newsFeedBellIconClass?: string,
};

export default class NewsFeedIcon extends Component<Props> {
  render() {
    const { onNewsFeedIconClick, newsFeedBellIconClass } = this.props;

    const componentClasses = classNames([
      styles.component,
      newsFeedBellIconClass,
    ]);

    return (
      <button className={componentClasses} onClick={onNewsFeedIconClick}>
        <SVGInline className={styles.icon} svg={newsFeedBellIcon} />
      </button>
    );
  }
}
