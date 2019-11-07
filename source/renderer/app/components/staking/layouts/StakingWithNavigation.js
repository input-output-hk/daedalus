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
  isActiveNavItem: Function,
  isIncentivizedTestnet: boolean,
};

type State = {
  scrollTop: number,
};

type ContextValue = {
  scrollTop: number,
};

export const StakingPageScrollContext = React.createContext<ContextValue>({
  scrollTop: 0,
});

@observer
export default class StakingWithNavigation extends Component<Props, State> {
  constructor(props: Props) {
    super(props);
    this.state = {
      scrollTop: 0,
    };
  }

  handleScroll = (evt: SyntheticEvent<HTMLElement>) => {
    this.setState({ scrollTop: evt.currentTarget.scrollTop });
  };

  render() {
    const {
      children,
      onNavItemClick,
      activeItem,
      isActiveNavItem,
      isIncentivizedTestnet,
    } = this.props;
    const { scrollTop } = this.state;

    return (
      <StakingPageScrollContext.Provider value={{ scrollTop }}>
        <div className={styles.component}>
          <div className={styles.navigation}>
            <StakingNavigation
              isActiveNavItem={isActiveNavItem}
              onNavItemClick={onNavItemClick}
              activeItem={activeItem}
              isIncentivizedTestnet={isIncentivizedTestnet}
            />
          </div>
          <div className={styles.page} onScroll={this.handleScroll}>
            {children}
          </div>
        </div>
      </StakingPageScrollContext.Provider>
    );
  }
}
