import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingPowerDelegation from '../../components/voting/voting-governance/VotingPowerDelegation';
import { VotingPowerDelegationConfirmationDialog } from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class VotingGovernancePage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { wallets, staking, app, voting } = this.props.stores;
    const { openExternalLink } = app;
    return (
      <VotingPowerDelegation
        onExternalLinkClick={openExternalLink}
        getFee={voting.calculateVotesDelegationFee}
        wallets={wallets.all}
        stakePools={staking.stakePools}
        getStakePoolById={staking.getStakePoolById}
        renderConfirmationDialog={({
          chosenOption,
          fees,
          onClose,
          selectedWallet,
        }) => (
          <VotingPowerDelegationConfirmationDialog
            chosenOption={chosenOption}
            fees={fees}
            onClose={onClose}
            onSubmit={(passphrase) =>
              voting.delegateVotes({
                chosenOption,
                passphrase,
                wallet: selectedWallet,
              })
            }
            selectedWallet={selectedWallet}
          />
        )}
      />
    );
  }
}

export default VotingGovernancePage;
