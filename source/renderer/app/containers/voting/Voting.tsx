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
  votingTabGovernance: {
    id: 'voting.tabs.governance',
    defaultMessage: '!!!Governance',
    description: 'Label for the governance voting tab.',
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
    const { app } = this.props.stores;
    const { intl } = this.props;
    const navItems: Array<NavButtonProps> = [
      {
        id: ROUTES.VOTING.GOVERNANCE,
        label: intl.formatMessage(messages.votingTabGovernance),
      },
      {
        id: ROUTES.VOTING.REGISTRATION,
        label: intl.formatMessage(messages.votingTabCatalyst),
      },
    ];
    const activeItem = navItems.find((item) => app.currentRoute === item.id);
    return (
      <MainLayout>
        <div style={{ height: '50px' }}>
          {environment.isMainnet || environment.isDev ? (
            <Navigation
              items={navItems}
              activeItem={activeItem.label}
              isActiveNavItem={(navItemId: string) =>
                navItemId === activeItem.id
              }
              onNavItemClick={(navItemId: string) => {
                this.props.actions.router.goToRoute.trigger({
                  route: navItemId,
                });
              }}
            />
          ) : null}
        </div>
        {this.props.children}
      </MainLayout>
    );
  }
}

export default injectIntl(Voting);
