// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import styles from './CountdownPartyIcon.scss';

type Props = {
  onIconClick: Function,
  iconClass?: string,
  shouldAnimate: boolean,
};

export default class CountdownPartyIcon extends Component<Props> {
  render() {
    const { onIconClick, iconClass, shouldAnimate } = this.props;
    const componentClasses = classNames([
      styles.component,
      shouldAnimate ? styles.animate : null,
      iconClass,
    ]);
    return (
      <button className={componentClasses} onClick={onIconClick}>
        <span role="img" aria-label="tada-icon">
          🎉
        </span>
      </button>
    );
  }
}
