// @flow
import React, { Component } from 'react';
import classNames from 'classnames';
import styles from './TadaButton.scss';

type Props = {
  onClick: Function,
  iconClass?: string,
  shouldAnimate: boolean,
};

export default class TadaButton extends Component<Props> {
  render() {
    const { onClick, iconClass, shouldAnimate } = this.props;
    const componentClasses = classNames([
      styles.component,
      shouldAnimate ? styles.animate : null,
      iconClass,
    ]);
    return (
      <button className={componentClasses} onClick={onClick}>
        <span role="img" aria-label="tada-icon">
          🎉
        </span>
      </button>
    );
  }
}
