import React, { Component, createRef } from 'react';
import { observer } from 'mobx-react';
import Navigation from '../../navigation/Navigation';
import type { NavButtonProps } from '../../navigation/Navigation';
import styles from './GovernanceWithNavigation.scss';

type ContextValue = {
  scrollElementRef: React.RefObject<HTMLDivElement> | null | undefined;
};

// The scrolling element is this layout's page div, not the window, so the
// virtualized lists inside a governance page need a handle on it. Staking
// publishes the same handle from its own layout for the same reason.
export const GovernancePageScrollContext = React.createContext<ContextValue>({
  scrollElementRef: null,
});

type Props = {
  items: Array<NavButtonProps>;
  activeItem?: string;
  isActiveNavItem: (item: string) => boolean;
  onNavItemClick: (item: string) => void;
  children?: React.ReactNode;
};

/**
 * The governance section's frame: its tabs, then the page beneath them.
 *
 * The counterpart to StakingWithNavigation, and it exists for the same reason.
 * The tabs have to sit flush against the top bar rather than inside the page's
 * own margins, which only happens when one component owns the arrangement.
 * Stories render this too, so what is reviewed is the layout that ships rather
 * than an imitation of it assembled per story.
 */
@observer
class GovernanceWithNavigation extends Component<Props> {
  governancePageScrollContext = {
    scrollElementRef: createRef<HTMLDivElement>(),
  };

  render() {
    const { items, activeItem, isActiveNavItem, onNavItemClick, children } =
      this.props;

    return (
      <GovernancePageScrollContext.Provider
        value={this.governancePageScrollContext}
      >
        <div className={styles.component}>
          <div className={styles.navigation}>
            <Navigation
              items={items}
              activeItem={activeItem}
              isActiveNavItem={isActiveNavItem}
              onNavItemClick={onNavItemClick}
            />
          </div>
          <div
            className={styles.page}
            ref={this.governancePageScrollContext.scrollElementRef}
          >
            {children}
          </div>
        </div>
      </GovernancePageScrollContext.Provider>
    );
  }
}

export default GovernanceWithNavigation;
