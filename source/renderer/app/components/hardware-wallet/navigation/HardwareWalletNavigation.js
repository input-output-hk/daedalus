// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { includes } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import {
  WALLET_NAV_IDS,
  ITN_LEGACY_WALLET_EXCLUDED_NAV_ITEMS,
} from '../../../config/walletNavigationConfig';
import Navigation from '../../navigation/Navigation';
import summaryIcon from '../../../assets/images/wallet-nav/summary-ic.inline.svg';
import type {
  NavButtonProps,
  NavDropdownProps,
} from '../../navigation/Navigation';

const messages = defineMessages({
  summary: {
    id: 'wallet.navigation.summary',
    defaultMessage: '!!!Summary',
    description: 'Label for the "Summary" nav button in the wallet navigation.',
  },
});

type Props = {
  activeItem: string,
  isActiveNavItem: Function,
  onNavItemClick: Function,
};

@observer
export default class WalletNavigation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActiveNavItem, onNavItemClick, activeItem } = this.props;
    const { intl } = this.context;
    const { isIncentivizedTestnet } = global;
    const items: Array<NavButtonProps | NavDropdownProps> = [
      {
        id: WALLET_NAV_IDS.SUMMARY,
        label: intl.formatMessage(messages.summary),
        icon: summaryIcon,
      },
    ].filter(
      item =>
        !(
          isIncentivizedTestnet &&
          includes(ITN_LEGACY_WALLET_EXCLUDED_NAV_ITEMS, item.id)
        )
    );
    return (
      <>
        {activeItem ? (
          <Navigation
            activeItem={activeItem}
            isActiveNavItem={isActiveNavItem}
            onNavItemClick={onNavItemClick}
            items={items}
          />
        ) : null}
      </>
    );
  }
}
