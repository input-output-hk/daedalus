import React, { Component, PropTypes } from 'react';
import classnames from 'classnames';
import styles from './BigButtonForDialogs.scss';

export default class BigButtonForDialogs extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    description: PropTypes.string.isRequired,
    icon: PropTypes.string.isRequired,
    onClick: PropTypes.func,
    isDisabled: PropTypes.bool
  };

  render() {
    const { label, description, icon, onClick, isDisabled = false } = this.props;
    const componentClasses = classnames([
      styles.component,
      isDisabled ? styles.disabled : null
    ]);
    return (
      <button
        className={componentClasses}
        onClick={onClick}
        disabled={isDisabled}
      >
        <img className={styles.icon} src={icon} role="presentation" />
        <div className={styles.label}>{label}</div>
        <div className={styles.description}>{description}</div>
      </button>
    );
  }
}
