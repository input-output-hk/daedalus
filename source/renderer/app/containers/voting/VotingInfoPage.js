// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import VotingInfo from '../../components/voting/info/VotingInfo';
import type { InjectedProps } from '../../types/injectedPropsType';
import { ROUTES } from '../../routes-config';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class VotingInfoPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions } = this.props;

    return (
      <VerticalFlexContainer>
        <VotingInfo
          onRegisterToVoteClick={() =>
            actions.router.goToRoute.trigger({ route: ROUTES.VOTING.ADD })
          }
        />
      </VerticalFlexContainer>
    );
  }
}
