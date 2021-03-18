// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import newsFeedIcon from '../../assets/images/top-bar/news-feed-icon.inline.svg';
import styles from './CountdownPartyIcon.scss';

type Props = {
  onIconClick: Function,
  iconClass?: string,
};

export default class CountdownPartyIcon extends Component<Props> {
  render() {
    const { onIconClick, iconClass } = this.props;
    const componentClasses = classNames([styles.component, iconClass]);
    return (
      <button className={componentClasses} onClick={onIconClick}>
        🎉
      </button>
    );
  }
}
