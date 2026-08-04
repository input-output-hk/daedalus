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
import { normalizeDRepIdentity } from '../../utils/governance/normalizeDRepIdentity';
import type { DRepIdentity } from '../../../../common/types/governance.types';
import type { VerifiedDRepNameSource } from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';

// The verified-off-chain label names the host that served the bytes; redirects
// are off, so the anchor URL's host is that host. A name whose host will not
// parse is dropped rather than labelled with a blank source.
const resolveVerifiedName = (
  verifiedName: string | null | undefined,
  anchorUrl: string | null | undefined
): VerifiedDRepNameSource | null => {
  if (verifiedName == null || anchorUrl == null) return null;
  try {
    return { host: new URL(anchorUrl).host, name: verifiedName };
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
    // The round trip carries wallet + vote type through GovernanceStore.delegationNavState.
    // Hash history v4 silently drops location.state on every push, so the store
    // observable is the only reliable transport across route boundaries.
    this.props.stores.governance.setDelegationNavState({
      from: ROUTES.VOTING.GOVERNANCE,
      selectedWalletId: formState.selectedWalletId,
      voteType: formState.voteType,
    });
    this.props.history.push(ROUTES.GOVERNANCE.DREPS);
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

    // GovernanceStore.delegationNavState is the sole transport for the DRep
    // selection round trip (location.state is not available in hash history v4).
    const initialFormState = governance.delegationNavState ?? undefined;

    return (
      <VotingPowerDelegation
        onExternalLinkClick={openExternalLink}
        initiateTransaction={voting.initializeVPDelegationTx}
        initialFormState={initialFormState}
        onBrowseDRepsClick={this.handleBrowseDRepsClick}
        onFetchDRep={(drepId) => governance.fetchDRep(drepId)}
        onEnsureFavorited={(drepId) => {
          if (!governance.favoriteDRepIds.has(drepId)) {
            governance.toggleFavorite(drepId);
          }
        }}
        wallets={wallets.all}
        stakePools={staking.stakePools}
        getStakePoolById={staking.getStakePoolById}
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
          // verifiedName is threaded through GovernanceStore.delegationNavState
          // by the directory/detail pages; it is only valid for the DRep that
          // was just selected.
          const verifiedName =
            isSentinel ||
            chosenOption !== initialFormState?.selectedDRepId
              ? null
              : resolveVerifiedName(
                  initialFormState.selectedDRepVerifiedName,
                  initialFormState.selectedDRepAnchorUrl
                );
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
