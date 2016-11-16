// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './WalletNavButton.scss';

@observer
export default class WalletNavButton extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    normalIcon: PropTypes.string.isRequired,
    activeIcon: PropTypes.string.isRequired,
    isActive: PropTypes.bool,
    className: PropTypes.string
  };

  render() {
    const { isActive, normalIcon, activeIcon } = this.props;
    const componentClasses = classNames([
      this.props.className, // allow to apply base classes from outside
      isActive ? styles.active : styles.normal
    ]);
    const iconUrl = isActive ? activeIcon : normalIcon;
    return (
      <div className={componentClasses}>
        <div className={styles.container}>
          <span className={styles.label}>{this.props.label}</span>
          <img className={styles.icon} src={iconUrl} role="presentation" />
        </div>
      </div>
    );
  }
}
