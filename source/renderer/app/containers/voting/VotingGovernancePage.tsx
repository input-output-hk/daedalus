import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingPowerDelegation from '../../components/voting/voting-governance/VotingPowerDelegation';
import VotingPowerDelegationConfirmationDialog from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';
import { ROUTES } from '../../routes-config';
import VotingUnavailable from '../../components/voting/VotingUnavailable';

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
      networkStatus,
    } = this.props.stores;
    const { openExternalLink } = app;
    const { isSynced, syncPercentage } = networkStatus;

    if (!isSynced) {
      return (
        <VotingUnavailable
          syncPercentage={syncPercentage}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ syncPercentage: any; onExternalLinkClick: ... Remove this comment to see the full error message
          onExternalLinkClick={openExternalLink}
        />
      );
    }

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
