import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape, injectIntl } from 'react-intl';
import Navigation from '../../components/navigation/Navigation';
import type { NavButtonProps } from '../../components/navigation/Navigation';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import MainLayout from '../MainLayout';
import { ROUTES } from '../../routes-config';

const messages = defineMessages({
  votingTabCatalyst: {
    id: 'voting.tabs.catalyst',
    defaultMessage: '!!!Catalyst Voting',
    description: 'Label for the catalyst voting tab.',
  },
});

type Props = InjectedContainerProps & {
  intl: intlShape.isRequired;
};

@inject('stores', 'actions')
@observer
class Voting extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    return <MainLayout>{this.props.children}</MainLayout>;
  }
}

export default injectIntl(Voting);
