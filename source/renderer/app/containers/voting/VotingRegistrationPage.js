// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import VotingInfo from '../../components/voting/VotingInfo';
import VotingUnavailable from '../../components/voting/VotingUnavailable';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingRegistrationDialogContainer from './dialogs/VotingRegistrationDialogContainer';
// import VotingRegistrationDialog from '../../components/voting/VotingRegistrationDialog';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class VotingRegistrationPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  onClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const { app, networkStatus, uiDialogs } = stores;
    const { openExternalLink } = app;
    const { isSynced, syncPercentage } = networkStatus;

    // @VOTING TODO: Improve it
    const isVotingRegistrationDialogOpen = uiDialogs.isOpen(
      'VotingRegistrationDialog'
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

    return (
      <Layout>
        <VerticalFlexContainer>
          <VotingInfo
            onRegisterToVoteClick={() =>
              actions.dialogs.open.trigger({
                dialog: 'VotingRegistrationDialog',
              })
            }
            onExternalLinkClick={openExternalLink}
          />
        </VerticalFlexContainer>

        {uiDialogs.isOpen('VotingRegistrationDialog') && (
          <VotingRegistrationDialogContainer onClose={this.onClose} />
        )}
      </Layout>
    );
  }
}
