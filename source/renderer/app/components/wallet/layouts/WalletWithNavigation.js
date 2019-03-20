// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

type Props = {
  children?: Node,
  isActiveScreen: Function,
  onWalletNavItemClick: Function,
  isSettingsPage: boolean,
};

@observer
export default class WalletWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      isActiveScreen,
      onWalletNavItemClick,
      isSettingsPage,
    } = this.props;
    const pageStyles = classnames([
      styles.page,
      isSettingsPage ? styles.settingsTabPage : null,
    ]);
    return (
      <div className={styles.component}>
        <div className={styles.navigation}>
          <WalletNavigation
            isActiveNavItem={isActiveScreen}
            onNavItemClick={onWalletNavItemClick}
          />
        </div>
        <div className={pageStyles}>{children}</div>
      </div>
    );
  }
}
