// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarWalletMenuItem.scss';
import disconnected from '../../../assets/images/sidebar/cross-light.svg';
import connected from '../../../assets/images/sidebar/checked-light.svg';
import connecting from '../../../assets/images/sidebar/spinner-light.svg';

@observer
export default class SidebarWalletMenuItem extends Component {

  static propTypes = {
    title: PropTypes.string.isRequired,
    info: PropTypes.string.isRequired,
    networkStatus: PropTypes.oneOf(['connected', 'connecting', 'disconnected']).isRequired,
    active: PropTypes.bool,
    onClick: PropTypes.func,
  };

  render() {
    const { title, info, active, onClick, networkStatus } = this.props;
    const componentStyles = classNames([ styles.component, active ? styles.active : null ]);
    const statusStyles = classNames([ styles.status, styles[networkStatus] ]);
    const statusIcons = { connected, connecting, disconnected };
    return (
      <button className={componentStyles} onClick={onClick}>
        <span className={styles.meta}>
          <span className={styles.title}>{title}</span>
          <span className={styles.info}>{info}</span>
        </span>
        <img className={statusStyles} src={statusIcons[networkStatus]} />
      </button>
    );
  }

}
