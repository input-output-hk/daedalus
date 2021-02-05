// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import { VOTING_REGISTRATION_MIN_WALLET_FUNDS } from '../../config/votingConfig';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import VotingInfo from '../../components/voting/VotingInfo';
import VotingNoWallets from '../../components/voting/VotingNoWallets';
import VotingUnavailable from '../../components/voting/VotingUnavailable';
import VotingRegistrationDialog from '../../components/voting/voting-registration-wizard-steps/widgets/VotingRegistrationDialog';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingRegistrationDialogContainer from './dialogs/VotingRegistrationDialogContainer';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class VotingRegistrationPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
  };

  render() {
    const { actions, stores } = this.props;
    const { app, networkStatus, uiDialogs, wallets } = stores;
    const { openExternalLink } = app;
    const { isSynced, syncPercentage } = networkStatus;

    const isVotingRegistrationDialogOpen = uiDialogs.isOpen(
      VotingRegistrationDialog
    );

    if (!isSynced && !isVotingRegistrationDialogOpen) {
      return (
        <Layout>
          <VerticalFlexContainer>
            <VotingUnavailable syncPercentage={syncPercentage} />
          </VerticalFlexContainer>
        </Layout>
      );
    }

    if (!wallets.allWallets.length) {
      return (
        <Layout>
          <VotingNoWallets
            onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
            minVotingFunds={VOTING_REGISTRATION_MIN_WALLET_FUNDS}
          />
        </Layout>
      );
    }

    return (
      <Layout>
        <VerticalFlexContainer>
          <VotingInfo
            onRegisterToVoteClick={() =>
              actions.dialogs.open.trigger({
                dialog: VotingRegistrationDialog,
              })
            }
            onExternalLinkClick={openExternalLink}
          />
        </VerticalFlexContainer>

        {isVotingRegistrationDialogOpen && (
          <VotingRegistrationDialogContainer />
        )}
      </Layout>
    );
  }
}
