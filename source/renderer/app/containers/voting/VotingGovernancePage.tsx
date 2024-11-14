import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingPowerDelegation from '../../components/voting/voting-governance/VotingPowerDelegation';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class VotingGovernancePage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { wallets, staking, app } = this.props.stores;
    const { openExternalLink } = app;
    return (
      <VotingPowerDelegation
        onExternalLinkClick={openExternalLink}
        wallets={wallets.all}
        stakePools={staking.stakePools}
        getStakePoolById={staking.getStakePoolById}
      />
    );
  }
}

export default VotingGovernancePage;
