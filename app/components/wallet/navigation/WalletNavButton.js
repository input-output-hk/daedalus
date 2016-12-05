// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletNavButton.scss';

@observer
export default class WalletNavButton extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    normalIcon: PropTypes.string.isRequired,
    activeIcon: PropTypes.string.isRequired,
    isActive: PropTypes.bool,
    onClick: PropTypes.func
  };

  render() {
    const { isActive, normalIcon, activeIcon, onClick } = this.props;
    const iconUrl = isActive ? activeIcon : normalIcon;
    return (
      <button className={isActive ? styles.active : styles.normal} onClick={onClick}>
        <div className={styles.container}>
          <span className={styles.label}>{this.props.label}</span>
          <img className={styles.icon} src={iconUrl} role="presentation" />
        </div>
      </button>
    );
  }
}
