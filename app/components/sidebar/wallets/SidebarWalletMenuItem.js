// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarWalletMenuItem.scss';
import connected from '../../../assets/images/sidebar/checked-light.svg';
import connecting from '../../../assets/images/sidebar/spinner-light.svg';

@observer
export default class SidebarWalletMenuItem extends Component {

  static propTypes = {
    title: PropTypes.string.isRequired,
    info: PropTypes.string.isRequired,
    isConnected: PropTypes.bool.isRequired,
    active: PropTypes.bool,
    onClick: PropTypes.func,
  };

  render() {
    const { title, info, active, onClick, isConnected } = this.props;
    const componentStyles = classNames([ styles.component, active ? styles.active : null ]);
    const statusStyles = classNames([
      styles.status,
      isConnected ? null : styles.connecting,
    ]);
    const statusIcon = isConnected ? connected : connecting;
    return (
      <button className={componentStyles} onClick={onClick}>
        <span className={styles.meta}>
          <span className={styles.title}>{title}</span>
          <span className={styles.info}>{info}</span>
        </span>
        <img className={statusStyles} src={statusIcon} />
      </button>
    );
  }

}
