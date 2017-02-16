// @flow
import React, { Component, PropTypes } from 'react';
import RTAppBar from 'react-toolbox/lib/app_bar/AppBar';
import { observer } from 'mobx-react';
import menuIcon from '../../assets/images/menu-ic.svg';
import styles from './AppBar.scss';

@observer
export default class AppBar extends Component {

  static propTypes = {
    onToggleSidebar: PropTypes.func.isRequired,
    children: PropTypes.element.isRequired,
  };

  render() {
    const sidebarToggleIcon = <img className={styles.sidebarIcon} src={menuIcon} role="presentation" />;
    return (
      <RTAppBar
        title="Daedalus"
        leftIcon={sidebarToggleIcon}
        onLeftIconClick={this.props.onToggleSidebar}
      >
        {this.props.children}
      </RTAppBar>
    );
  }
}
