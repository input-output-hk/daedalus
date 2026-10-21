import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { withRouter } from 'react-router-dom';
import type { RouteComponentProps } from 'react-router-dom';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingPowerDelegation from '../../components/voting/voting-governance/VotingPowerDelegation';
import { ROUTES } from '../../routes-config';
import VotingUnavailable from '../../components/voting/VotingUnavailable';
import type { VoteType } from '../../components/voting/voting-governance/types';
import VotingPowerDelegationConfirmationDialog from '../../components/voting/voting-governance/VotingPowerDelegationConfirmationDialog';
import { normalizeDRepIdentity } from '../../utils/governance/normalizeDRepIdentity';

type Props = InjectedProps & RouteComponentProps;

@inject('stores', 'actions')
@observer
class VotingGovernancePage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  private preparingHardwareGovernance = false;

  componentWillUnmount() {
    const { hardwareWallets } = this.props.stores;
    if (!global.cip30Enabled && this.preparingHardwareGovernance) {
      hardwareWallets._resetTransaction();
    }
  }

  // Back to where the flow starts, not back one step: the previous screen is
  // the directory, and returning there would offer the same choice again to
  // someone who has just declined to make it.
  handleCancel = () => {
    this.props.stores.governance.setDelegationNavState(null);
    this.props.history.push(ROUTES.GOVERNANCE.DASHBOARD);
  };

  handleBrowseDRepsClick = (formState: {
    selectedWalletId: string | null;
    voteType: VoteType;
  }) => {
    // The round trip carries wallet + vote type through GovernanceStore.delegationNavState.
    // Hash history v4 silently drops location.state on every push, so the store
    // observable is the only reliable transport across route boundaries.
    this.props.stores.governance.setDelegationNavState({
      from: ROUTES.GOVERNANCE.DELEGATE,
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
        submitTransaction={voting.delegateVotes}
        initiateTransaction={(params) => {
          this.preparingHardwareGovernance = params.wallet.isHardwareWallet;
          return voting.initializeVPDelegationTx(params);
        }}
        initialFormState={initialFormState}
        onBrowseDRepsClick={this.handleBrowseDRepsClick}
        onCancel={this.handleCancel}
        onFetchDRep={(drepId) => governance.fetchDRep(drepId)}
        onEnsureFavorited={(drepId) => {
          if (!governance.favoriteDRepIds.has(drepId)) {
            governance.toggleFavorite(drepId);
          }
        }}
        wallets={wallets.all}
        stakePools={staking.stakePools}
        getStakePoolById={staking.getStakePoolById}
        onSuccess={(id) => {
          this.props.actions.router.goToRoute.trigger({
            route: ROUTES.WALLETS.SUMMARY,
            params: { id },
          });
        }}
        renderConfirmationDialog={({
          chosenOption,
          fees,
          onClose,
          selectedWallet,
        }) => {
          const isSentinel =
            chosenOption === 'abstain' || chosenOption === 'no_confidence';
          let verifiedName = null;
          if (
            !isSentinel &&
            chosenOption === initialFormState?.selectedDRepId &&
            initialFormState.selectedDRepVerifiedName &&
            initialFormState.selectedDRepAnchorUrl
          ) {
            try {
              verifiedName = {
                host: new URL(initialFormState.selectedDRepAnchorUrl).host,
                name: initialFormState.selectedDRepVerifiedName,
              };
            } catch {
              // An invalid source cannot establish verified-name provenance.
            }
          }
          return (
            <VotingPowerDelegationConfirmationDialog
              chosenOption={chosenOption}
              drepIdentity={
                isSentinel ? null : normalizeDRepIdentity(chosenOption)
              }
              fees={fees}
              hwDeviceStatus={hardwareWallets.hwDeviceStatus}
              isTrezor={hardwareWallets.checkIsTrezorByWalletId(
                selectedWallet.id
              )}
              onClose={() => {
                if (selectedWallet.isHardwareWallet)
                  hardwareWallets._resetTransaction();
                onClose();
              }}
              onExternalLinkClick={openExternalLink}
              onSubmit={async (passphrase) => {
                const result = await voting.delegateVotes({
                  chosenOption,
                  passphrase,
                  wallet: selectedWallet,
                });
                if (
                  result.success &&
                  !isSentinel &&
                  !governance.favoriteDRepIds.has(chosenOption)
                )
                  governance.toggleFavorite(chosenOption);
                return result;
              }}
              redirectToWallet={(id) => {
                this.props.actions.router.goToRoute.trigger({
                  route: ROUTES.WALLETS.SUMMARY,
                  params: { id },
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
