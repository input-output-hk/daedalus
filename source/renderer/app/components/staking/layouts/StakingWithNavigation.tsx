import React, { Component, createRef } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { ElementRef, Node } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import StakingNavigation from '../navigation/StakingNavigation';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingWithNavigation.scss' ... Remove this comment to see the full error message
import styles from './StakingWithNavigation.scss';

type Props = {
  children?: Node;
  activeItem: string;
  showInfoTab: boolean;
  onNavItemClick: (...args: Array<any>) => any;
  isActiveNavItem: (...args: Array<any>) => any;
};
type ContextValue = {
  scrollElementRef: ElementRef<any> | null | undefined;
};
export const StakingPageScrollContext = React.createContext<ContextValue>({
  scrollElementRef: null,
});

@observer
class StakingWithNavigation extends Component<Props> {
  stakingPageRef = createRef<any>();

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
        value={{
          scrollElementRef: this.stakingPageRef,
        }}
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
              // @ts-ignore ts-migrate(2540) FIXME: Cannot assign to 'current' because it is a read-on... Remove this comment to see the full error message
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

export default StakingWithNavigation;
