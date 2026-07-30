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
import { normalizeDRepIdentity } from '../../utils/governance/normalizeDRepIdentity';
import type { DRepIdentity } from '../../../../common/types/governance.types';
import type { AppDRepDirectoryEntry } from '../../stores/GovernanceStore';
import type { VerifiedDRepNameSource } from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';

// The verified-off-chain label names the host that served the bytes; redirects
// are off, so the anchor URL's host is that host. A name whose host will not
// parse is dropped rather than labelled with a blank source.
const resolveVerifiedName = (
  entry: AppDRepDirectoryEntry | undefined
): VerifiedDRepNameSource | null => {
  if (entry?.verifiedName == null || entry.anchor == null) return null;
  try {
    return { host: new URL(entry.anchor.url).host, name: entry.verifiedName };
  } catch {
    return null;
  }
};

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
    const {
      wallets,
      staking,
      app,
      voting,
      hardwareWallets,
      networkStatus,
      governance,
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
        drepIndex={governance.drepIndex}
        renderConfirmationDialog={({
          chosenOption,
          fees,
          onClose,
          selectedWallet,
        }) => {
          // Sentinels carry no identity; a drep target is decoded for display
          // only — the rendered and submitted string stays chosenOption itself,
          // untouched.
          const isSentinel =
            chosenOption === 'abstain' || chosenOption === 'no_confidence';
          const drepIdentity: DRepIdentity | null = isSentinel
            ? null
            : normalizeDRepIdentity(chosenOption);
          const verifiedName = isSentinel
            ? null
            : resolveVerifiedName(governance.drepIndex.get(chosenOption));
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
              verifiedName={verifiedName}
            />
          );
        }}
      />
    );
  }
}

export default withRouter(VotingGovernancePage);
