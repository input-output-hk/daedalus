import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { includes } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import {
  WALLET_NAV_IDS,
  LEGACY_WALLET_EXCLUDED_NAV_ITEMS,
} from '../../../config/walletNavigationConfig';
import Navigation from '../../navigation/Navigation';
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
  send: {
    id: 'wallet.navigation.send',
    defaultMessage: '!!!Send',
    description: 'Label for the "Send" nav button in the wallet navigation.',
  },
  receive: {
    id: 'wallet.navigation.receive',
    defaultMessage: '!!!Receive',
    description: 'Label for the "Receive" nav button in the wallet navigation.',
  },
  transactions: {
    id: 'wallet.navigation.transactions',
    defaultMessage: '!!!Transactions',
    description:
      'Label for the "Transactions" nav button in the wallet navigation.',
  },
  tokens: {
    id: 'wallet.navigation.tokens',
    defaultMessage: '!!!Tokens',
    description: 'Label for the "Tokens" nav button in the wallet navigation.',
  },
  settings: {
    id: 'wallet.navigation.settings',
    defaultMessage: '!!!Settings',
    description:
      'Label for the "Settings" nav button in the wallet navigation.',
  },
  utxo: {
    id: 'wallet.navigation.utxo',
    defaultMessage: '!!!Wallet UTXO distribution',
    description:
      'Label for the "Wallet UTXO distribution" nav button in the wallet navigation.',
  },
  more: {
    id: 'wallet.navigation.more',
    defaultMessage: '!!!More',
    description: 'Label for the "More" nav button in the wallet navigation.',
  },
});
type Props = {
  activeItem: string;
  isActiveNavItem: (...args: Array<any>) => any;
  isLegacy: boolean;
  onNavItemClick: (...args: Array<any>) => any;
  hasNotification?: boolean;
};

@observer
class WalletNavigation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      isActiveNavItem,
      isLegacy,
      onNavItemClick,
      activeItem,
      hasNotification,
    } = this.props;
    const { intl } = this.context;
    // @ts-ignore ts-migrate(2322) FIXME: Type '({ id: string; label: any; type?: undefined;... Remove this comment to see the full error message
    const items: Array<NavButtonProps | NavDropdownProps> = [
      {
        id: WALLET_NAV_IDS.SUMMARY,
        label: intl.formatMessage(messages.summary),
      },
      {
        id: WALLET_NAV_IDS.SEND,
        label: intl.formatMessage(messages.send),
      },
      {
        id: WALLET_NAV_IDS.RECEIVE,
        label: intl.formatMessage(messages.receive),
      },
      {
        id: WALLET_NAV_IDS.TRANSACTIONS,
        label: intl.formatMessage(messages.transactions),
      },
      {
        id: WALLET_NAV_IDS.TOKENS,
        label: intl.formatMessage(messages.tokens),
      },
      {
        id: WALLET_NAV_IDS.SETTINGS,
        type: 'dropdown',
        label: intl.formatMessage(messages.more),
        hasNotification,
        options: [
          {
            label: intl.formatMessage(messages.settings),
            value: 'settings',
            hasNotification,
          },
          {
            label: intl.formatMessage(messages.utxo),
            value: 'utxo',
          },
        ],
      },
    ].filter(
      (item) =>
        !(isLegacy && includes(LEGACY_WALLET_EXCLUDED_NAV_ITEMS, item.id))
    );
    return (
      <Navigation
        activeItem={activeItem}
        isActiveNavItem={isActiveNavItem}
        onNavItemClick={onNavItemClick}
        items={items}
      />
    );
  }
}

export default WalletNavigation;
