import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import classnames from 'classnames';
import styles from './BigButtonForDialogs.scss';

export default class BigButtonForDialogs extends Component {

  props: {
    label: string,
    description: string,
    icon: string,
    onClick: Function,
    isDisabled: boolean,
    className: string,
  };

  render() {
    const { label, description, icon, onClick, isDisabled = false, className } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isDisabled ? styles.disabled : null
    ]);
    return (
      <button
        className={componentClasses}
        onClick={onClick}
        disabled={isDisabled}
      >
        <SvgInline svg={icon} className={styles.icon} />
        <div className={styles.label}>{label}</div>
        <div className={styles.description}>{description}</div>
      </button>
    );
  }
}
