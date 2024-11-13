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
    const { openExternalLink } = this.props.stores.app;
    return <VotingPowerDelegation onExternalLinkClick={openExternalLink} />;
  }
}

export default VotingGovernancePage;
