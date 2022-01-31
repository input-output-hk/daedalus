import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import { VOTING_REGISTRATION_MIN_WALLET_FUNDS } from '../../config/votingConfig';
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
class VotingRegistrationPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.ADD,
    });
  };
  getInnerContent = (isVotingRegistrationDialogOpen: boolean) => {
    const { app, networkStatus, wallets, profile, voting } = this.props.stores;
    const { isSynced, syncPercentage } = networkStatus;
    const { openExternalLink } = app;

    if (!isSynced && !isVotingRegistrationDialogOpen) {
      return (
        <VotingUnavailable
          syncPercentage={syncPercentage}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ syncPercentage: any; onExternalLinkClick: ... Remove this comment to see the full error message
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
        fundPhase={voting.fundPhase}
        currentLocale={currentLocale}
        currentDateFormat={currentDateFormat}
        currentTimeFormat={currentTimeFormat}
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

export default VotingRegistrationPage;
