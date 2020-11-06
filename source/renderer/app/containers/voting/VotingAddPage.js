// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import VotingInfo from '../../components/voting/info/VotingInfo';
import type { InjectedProps } from '../../types/injectedPropsType';
import VotingAddDialogContainer from './dialogs/VotingAddDialogContainer';
import VotingAddDialog from '../../components/voting/VotingAddDialog';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class VotingAddPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  onClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const { uiDialogs } = stores;

    return (
      <MainLayout>
        <VerticalFlexContainer>
          <VotingInfo
            onRegisterToVoteClick={() =>
              actions.dialogs.open.trigger({ dialog: VotingAddDialog })
            }
          />
        </VerticalFlexContainer>
        {uiDialogs.isOpen(VotingAddDialog) && (
          <VotingAddDialogContainer onClose={this.onClose} />
        )}
      </MainLayout>
    );
  }
}
