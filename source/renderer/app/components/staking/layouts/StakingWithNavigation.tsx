import React, { Component, createRef } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import StakingNavigation from '../navigation/StakingNavigation';
import styles from './StakingWithNavigation.scss';

type Props = {
  children?: React.ReactNode;
  activeItem: string;
  showInfoTab: boolean;
  onNavItemClick: (...args: Array<any>) => any;
  isActiveNavItem: (...args: Array<any>) => any;
};
type ContextValue = {
  scrollElementRef: React.RefObject<HTMLDivElement> | null | undefined;
};
export const StakingPageScrollContext = React.createContext<ContextValue>({
  scrollElementRef: null,
});

@observer
class StakingWithNavigation extends Component<Props> {
  stakingPageScrollContext = { scrollElementRef: createRef<HTMLDivElement>() };

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
      <StakingPageScrollContext.Provider value={this.stakingPageScrollContext}>
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
            ref={this.stakingPageScrollContext.scrollElementRef}
          >
            {children}
          </div>
        </div>
      </StakingPageScrollContext.Provider>
    );
  }
}

export default StakingWithNavigation;
