// @flow
import React, { Component, PropTypes } from 'react';
import AppBar from 'react-toolbox/lib/app_bar/AppBar';
import { observer } from 'mobx-react';
import Sidebar from '../sidebar/Sidebar';
import styles from './SidebarLayout.scss';
import menuIcon from '../../assets/images/menu-ic.svg';

@observer
export default class SidebarLayout extends Component {

  static propTypes = {
    children: PropTypes.arrayOf(PropTypes.element).isRequired,
    sidebarHidden: PropTypes.bool.isRequired
  };

  render() {
    const { sidebarHidden, children } = this.props;
    const sidebarToggleIcon = <img className={styles.sidebarIcon} src={menuIcon} role="presentation" />;
    return (
      <div className={styles.component}>

        <Sidebar hidden={sidebarHidden} routePath="/wallets" />

        <div className={styles.main}>
          <AppBar
            title="Daedalus"
            leftIcon={sidebarToggleIcon}
          />
          {children}
        </div>

      </div>
    );
  }
}
