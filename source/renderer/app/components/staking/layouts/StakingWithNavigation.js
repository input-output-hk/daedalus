// @flow
import React, { Component, createRef } from 'react';
import type { ElementRef, Node } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import StakingNavigation from '../navigation/StakingNavigation';
import styles from './StakingWithNavigation.scss';

type Props = {
  children?: Node,
  activeItem: string,
  showInfoTab: boolean,
  onNavItemClick: Function,
  isActiveNavItem: Function,
};

type ContextValue = {
  scrollElementRef: ?ElementRef<*>,
};

export const StakingPageScrollContext = React.createContext<ContextValue>({
  scrollElementRef: null,
});

@observer
export default class StakingWithNavigation extends Component<Props> {
  stakingPageRef = createRef<*>();

  render() {
    const {
      children,
      onNavItemClick,
      activeItem,
      isActiveNavItem,
      showInfoTab,
    } = this.props;
    const componentStyles = classnames([styles.component, styles[activeItem]]);

    return (
      <StakingPageScrollContext.Provider
        value={{ scrollElementRef: this.stakingPageRef }}
      >
        <div className={componentStyles}>
          <div className={styles.navigation}>
            <StakingNavigation
              isActiveNavItem={isActiveNavItem}
              onNavItemClick={onNavItemClick}
              activeItem={activeItem}
              showInfoTab={showInfoTab}
            />
          </div>
          <div
            className={styles.page}
            ref={(ref) => {
              this.stakingPageRef.current = ref;
            }}
          >
            {children}
          </div>
        </div>
      </StakingPageScrollContext.Provider>
    );
  }
}
