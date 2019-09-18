// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import newsFeedIcon from '../../assets/images/top-bar/news-feed-icon.inline.svg';
import styles from './NewsFeedIcon.scss';

type Props = {
  onNewsFeedIconClick: Function,
  newsFeedIconClass?: string,
};

export default class NewsFeedIcon extends Component<Props> {
  render() {
    const { onNewsFeedIconClick, newsFeedIconClass } = this.props;

    const componentClasses = classNames([styles.component, newsFeedIconClass]);

    return (
      <button className={componentClasses} onClick={onNewsFeedIconClick}>
        <SVGInline className={styles.icon} svg={newsFeedIcon} />
      </button>
    );
  }
}
