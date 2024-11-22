import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingPowerDelegation from '../../components/voting/voting-governance/VotingPowerDelegation';
import VotingPowerDelegationConfirmationDialog from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';
import { ROUTES } from '../../routes-config';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class VotingGovernancePage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const {
      wallets,
      staking,
      app,
      voting,
      hardwareWallets,
    } = this.props.stores;
    const { openExternalLink } = app;
    return (
      <VotingPowerDelegation
        onExternalLinkClick={openExternalLink}
        initiateTransaction={voting.initializeVPDelegationTx}
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
            hwDeviceStatus={hardwareWallets.hwDeviceStatus}
            isTrezor={hardwareWallets.checkIsTrezorByWalletId(
              selectedWallet.id
            )}
            onClose={onClose}
            onExternalLinkClick={openExternalLink}
            onSubmit={(passphrase) =>
              voting.delegateVotes({
                chosenOption,
                passphrase,
                wallet: selectedWallet,
              })
            }
            redirectToWallet={(id) => {
              this.props.actions.router.goToRoute.trigger({
                route: ROUTES.WALLETS.SUMMARY,
                params: {
                  id,
                },
              });
            }}
            selectedWallet={selectedWallet}
          />
        )}
      />
    );
  }
}

export default VotingGovernancePage;
