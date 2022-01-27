import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/top-bar/ne... Remove this comment to see the full error message
import newsFeedIcon from '../../assets/images/top-bar/news-feed-icon.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NewsFeedIcon.scss' or its co... Remove this comment to see the full error message
import styles from './NewsFeedIcon.scss';

type Props = {
  onNewsFeedIconClick: (...args: Array<any>) => any;
  newsFeedIconClass?: string;
  hasNotification: boolean;
  hasUpdate: boolean;
};
export default class NewsFeedIcon extends Component<Props> {
  render() {
    const {
      onNewsFeedIconClick,
      newsFeedIconClass,
      hasNotification,
      hasUpdate,
    } = this.props;
    const componentClasses = classNames([
      styles.component,
      hasNotification && !hasUpdate ? styles.notificationDot : null,
      hasUpdate ? styles.updateDot : null,
      newsFeedIconClass,
    ]);
    return (
      <button className={componentClasses} onClick={onNewsFeedIconClick}>
        <SVGInline className={styles.icon} svg={newsFeedIcon} />
      </button>
    );
  }
}
