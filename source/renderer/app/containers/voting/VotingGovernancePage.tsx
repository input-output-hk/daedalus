import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingPowerDelegation from '../../components/voting/voting-governance/VotingPowerDelegation';
import VotingPowerDelegationConfirmationDialog from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';
import { ROUTES } from '../../routes-config';
import VotingUnavailable from '../../components/voting/VotingUnavailable';
import type { VoteType } from '../../components/voting/voting-governance/types';
import { pickDelegationFormNavigationState } from '../governance/delegationFormState';
import type { DRepIdentity } from '../../../../common/types/governance.types';

type Props = InjectedProps & RouteComponentProps;

@inject('stores', 'actions')
@observer
class VotingGovernancePage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  handleBrowseDRepsClick = (formState: {
    selectedWalletId: string | null;
    voteType: VoteType;
  }) => {
    // The round trip carries wallet + vote type out and back through
    // location.state only (invariant: no second delegation backend).
    this.props.history.push(ROUTES.GOVERNANCE.DREPS, {
      from: ROUTES.VOTING.GOVERNANCE,
      selectedWalletId: formState.selectedWalletId,
      voteType: formState.voteType,
    });
  };

  render() {
    const { wallets, staking, app, voting, hardwareWallets, networkStatus } =
      this.props.stores;
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

    const initialFormState = pickDelegationFormNavigationState(
      this.props.location.state
    );

    return (
      <VotingPowerDelegation
        onExternalLinkClick={openExternalLink}
        initiateTransaction={voting.initializeVPDelegationTx}
        initialFormState={initialFormState}
        onBrowseDRepsClick={this.handleBrowseDRepsClick}
        wallets={wallets.all}
        stakePools={staking.stakePools}
        getStakePoolById={staking.getStakePoolById}
        renderConfirmationDialog={({
          chosenOption,
          fees,
          onClose,
          selectedWallet,
        }) => {
          // Sentinels render as labels; a drep target renders its raw ID.
          // credentialType is a syntactic classification only — the rendered
          // and submitted string is chosenOption itself, untouched.
          const drepIdentity: DRepIdentity | null =
            chosenOption === 'abstain' || chosenOption === 'no_confidence'
              ? null
              : {
                  credentialType: chosenOption.startsWith('drep_script')
                    ? 'script'
                    : 'key',
                  raw: chosenOption,
                };
          return (
            <VotingPowerDelegationConfirmationDialog
              chosenOption={chosenOption}
              drepIdentity={drepIdentity}
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
          );
        }}
      />
    );
  }
}

export default withRouter(VotingGovernancePage);
