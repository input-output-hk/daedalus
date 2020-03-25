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
import sendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
import receiveIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
import transactionsIcon from '../../../assets/images/wallet-nav/transactions-ic.inline.svg';
import settingsIcon from '../../../assets/images/wallet-nav/wallet-settings-2-ic.inline.svg';
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
  activeItem: string,
  isActiveNavItem: Function,
  isLegacy: boolean,
  onNavItemClick: Function,
  hasNotification?: boolean,
};

@observer
export default class WalletNavigation extends Component<Props> {
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
    const { isIncentivizedTestnet } = global;
    const items: Array<NavButtonProps | NavDropdownProps> = [
      {
        id: WALLET_NAV_IDS.SUMMARY,
        label: intl.formatMessage(messages.summary),
        icon: summaryIcon,
      },
      {
        id: WALLET_NAV_IDS.SEND,
        label: intl.formatMessage(messages.send),
        icon: sendIcon,
      },
      {
        id: WALLET_NAV_IDS.RECEIVE,
        label: intl.formatMessage(messages.receive),
        icon: receiveIcon,
      },
      {
        id: WALLET_NAV_IDS.TRANSACTIONS,
        label: intl.formatMessage(messages.transactions),
        icon: transactionsIcon,
      },
      {
        id: WALLET_NAV_IDS.SETTINGS,
        type: 'dropdown',
        label:
          isLegacy && isIncentivizedTestnet
            ? intl.formatMessage(messages.settings)
            : intl.formatMessage(messages.more),
        icon: settingsIcon,
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
      item =>
        !(
          isIncentivizedTestnet &&
          isLegacy &&
          includes(ITN_LEGACY_WALLET_EXCLUDED_NAV_ITEMS, item.id)
        )
    );
    return (
      <Navigation
        activeItem={activeItem}
        isActiveNavItem={isActiveNavItem}
        onNavItemClick={onNavItemClick}
        isLegacy={isLegacy}
        items={items}
      />
    );
  }
}
