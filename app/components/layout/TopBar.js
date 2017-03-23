// @flow
import React, { Component, PropTypes } from 'react';
import RTAppBar from 'react-toolbox/lib/app_bar/AppBar';
import { observer } from 'mobx-react';
import menuIcon from '../../assets/images/menu-ic.svg';
import styles from './TopBar.scss';

@observer
export default class TopBar extends Component {

  static propTypes = {
    onToggleSidebar: PropTypes.func,
    children: PropTypes.element,
  };

  render() {
    const { onToggleSidebar } = this.props;
    const sidebarToggleIcon = onToggleSidebar && <img className={styles.sidebarIcon} src={menuIcon} role="presentation" />;
    return (
      <RTAppBar
        title="Daedalus"
        leftIcon={sidebarToggleIcon}
        onLeftIconClick={onToggleSidebar}
      >
        {this.props.children}
      </RTAppBar>
    );
  }
}
