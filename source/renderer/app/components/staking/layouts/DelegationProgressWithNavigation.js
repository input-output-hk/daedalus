// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import DelegationProgressNavigation from '../navigation/DelegationProgressNavigation';
import styles from './DelegationProgressWithNavigation.scss';

type Props = {
  children?: Node,
  isActiveScreen: Function,
  onDelegationProgressNavItemClick: Function,
};

@observer
export default class DelegationProgressWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      isActiveScreen,
      onDelegationProgressNavItemClick,
    } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.navigation}>
          <DelegationProgressNavigation
            isActiveNavItem={isActiveScreen}
            onNavItemClick={onDelegationProgressNavItemClick}
          />
        </div>
        <div className={styles.page}>{children}</div>
      </div>
    );
  }
}
