// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import {
  IS_VOTING_REGISTRATION_AVAILABLE,
  VOTING_REGISTRATION_MIN_WALLET_FUNDS,
} from '../../config/votingConfig';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import VotingInfo from '../../components/voting/voting-info/VotingInfo';
import VotingNoWallets from '../../components/voting/VotingNoWallets';
import VotingUnavailable from '../../components/voting/VotingUnavailable';
import VotingRegistrationDialog from '../../components/voting/voting-registration-wizard-steps/widgets/VotingRegistrationDialog';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingRegistrationDialogContainer from './dialogs/VotingRegistrationDialogContainer';
import { VotingFooterLinks } from '../../components/voting/VotingFooterLinks';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class VotingRegistrationPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD });
  };

  getInnerContent = (isVotingRegistrationDialogOpen: boolean) => {
    const { app, networkStatus, wallets, voting, profile } = this.props.stores;
    const { isSynced, syncPercentage } = networkStatus;
    const { isRegistrationEnded } = voting;
    const { openExternalLink } = app;

    if (
      !IS_VOTING_REGISTRATION_AVAILABLE ||
      (!isSynced && !isVotingRegistrationDialogOpen)
    ) {
      return (
        <VotingUnavailable
          syncPercentage={syncPercentage}
          isVotingRegistrationAvailable={IS_VOTING_REGISTRATION_AVAILABLE}
          onExternalLinkClick={openExternalLink}
        />
      );
    }

    if (!wallets.allWallets.length) {
      return (
        <VotingNoWallets
          onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
          minVotingFunds={VOTING_REGISTRATION_MIN_WALLET_FUNDS}
        />
      );
    }

    const { currentTimeFormat, currentDateFormat, currentLocale } = profile;
    return (
      <VotingInfo
        currentLocale={currentLocale}
        currentDateFormat={currentDateFormat}
        currentTimeFormat={currentTimeFormat}
        isRegistrationEnded={isRegistrationEnded}
        onRegisterToVoteClick={() =>
          this.props.actions.dialogs.open.trigger({
            dialog: VotingRegistrationDialog,
          })
        }
        onExternalLinkClick={openExternalLink}
      />
    );
  };

  render() {
    const { stores } = this.props;
    const { app, uiDialogs } = stores;
    const { openExternalLink } = app;

    const isVotingRegistrationDialogOpen = uiDialogs.isOpen(
      VotingRegistrationDialog
    );

    const innerContent = this.getInnerContent(isVotingRegistrationDialogOpen);

    return (
      <Layout>
        <VerticalFlexContainer>
          {innerContent}
          <VotingFooterLinks onClickExternalLink={openExternalLink} />
        </VerticalFlexContainer>

        {isVotingRegistrationDialogOpen && (
          <VotingRegistrationDialogContainer />
        )}
      </Layout>
    );
  }
}
