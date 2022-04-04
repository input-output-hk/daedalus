import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Navigation from '../../navigation/Navigation';

const messages = defineMessages({
  delegation_center: {
    id: 'staking.navigation.delegation_center',
    defaultMessage: '!!!Delegation center',
    description:
      'Label for the "Delegation" nav button in the staking navigation.',
  },
  stake_pools: {
    id: 'staking.navigation.stake_pools',
    defaultMessage: '!!!Stake pools',
    description: 'Label for the "Stake" nav button in the staking navigation.',
  },
  rewards: {
    id: 'staking.navigation.rewards',
    defaultMessage: '!!!Rewards',
    description:
      'Label for the "Rewards" nav button in the staking navigation.',
  },
  epochs: {
    id: 'staking.navigation.epochs',
    defaultMessage: '!!!Epochs',
    description: 'Label for the "Epochs" nav button in the staking navigation.',
  },
  info: {
    id: 'staking.navigation.info',
    defaultMessage: '!!!Info',
    description: 'Label for the "Info" nav button in the staking navigation.',
  },
});
type Props = {
  activeItem: string;
  showInfoTab: boolean;
  onNavItemClick: (...args: Array<any>) => any;
  isActiveNavItem: (...args: Array<any>) => any;
};

@observer
class StakingNavigation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      onNavItemClick,
      activeItem,
      isActiveNavItem,
      showInfoTab,
    } = this.props;
    const { intl } = this.context;
    const navigationItems = [
      {
        id: 'delegation-center',
        label: intl.formatMessage(messages.delegation_center),
      },
      {
        id: 'stake-pools',
        label: intl.formatMessage(messages.stake_pools),
      },
      {
        id: 'rewards',
        label: intl.formatMessage(messages.rewards),
      }, // {
      //   id: 'epochs',
      //   label: intl.formatMessage(messages.epochs),
      // },
    ];

    if (showInfoTab) {
      navigationItems.push({
        id: 'info',
        label: intl.formatMessage(messages.info),
      });
    }

    return (
      <Navigation
        activeItem={activeItem}
        isActiveNavItem={isActiveNavItem}
        onNavItemClick={onNavItemClick}
        items={navigationItems}
      />
    );
  }
}

export default StakingNavigation;
