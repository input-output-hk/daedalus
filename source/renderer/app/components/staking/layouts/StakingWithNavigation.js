// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import StakingNavigation from '../navigation/StakingNavigation';
import styles from './StakingWithNavigation.scss';

type Props = {
  children?: Node,
  activeItem: string,
  onNavItemClick: Function,
};

@observer
export default class StakingWithNavigation extends Component<Props> {
  render() {
    const { children, onNavItemClick, activeItem } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.navigation}>
          <StakingNavigation
            onNavItemClick={onNavItemClick}
            activeItem={activeItem}
          />
        </div>
        <div className={styles.page}>{children}</div>
      </div>
    );
  }
}
