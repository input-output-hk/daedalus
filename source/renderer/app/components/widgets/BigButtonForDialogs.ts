// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import styles from './BigButtonForDialogs.scss';

type Props = {
  label: string,
  description: string,
  icon: string,
  onClick?: Function,
  isDisabled?: boolean,
  className?: string,
};

export default class BigButtonForDialogs extends Component<Props> {
  render() {
    const {
      label,
      description,
      icon,
      onClick,
      isDisabled = false,
      className,
    } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isDisabled ? styles.disabled : null,
    ]);
    return (
      <button
        className={componentClasses}
        onClick={() => !isDisabled && onClick && onClick()}
      >
        <SVGInline svg={icon} className={styles.icon} />
        <div className={styles.label}>{label}</div>
        <div className={styles.description}>{description}</div>
      </button>
    );
  }
}
