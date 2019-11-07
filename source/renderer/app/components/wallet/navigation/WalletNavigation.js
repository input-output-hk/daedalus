// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Navigation from '../../navigation/Navigation';
import summaryIcon from '../../../assets/images/wallet-nav/summary-ic.inline.svg';
import sendIcon from '../../../assets/images/wallet-nav/send-ic.inline.svg';
import receiveIcon from '../../../assets/images/wallet-nav/receive-ic.inline.svg';
import transactionsIcon from '../../../assets/images/wallet-nav/transactions-ic.inline.svg';
import settingsIcon from '../../../assets/images/wallet-nav/wallet-settings-2-ic.inline.svg';

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
    return (
      <Navigation
        activeItem={activeItem}
        isActiveNavItem={isActiveNavItem}
        onNavItemClick={onNavItemClick}
        items={[
          {
            id: 'summary',
            label: intl.formatMessage(messages.summary),
            icon: summaryIcon,
          },
          {
            id: 'send',
            label: intl.formatMessage(messages.send),
            icon: sendIcon,
            isLegacy,
          },
          {
            id: 'receive',
            label: intl.formatMessage(messages.receive),
            icon: receiveIcon,
            isLegacy,
          },
          {
            id: 'transactions',
            label: intl.formatMessage(messages.transactions),
            icon: transactionsIcon,
          },
          {
            type: 'dropdown',
            id: 'settings',
            label: isLegacy
              ? intl.formatMessage(messages.settings)
              : intl.formatMessage(messages.more),
            icon: settingsIcon,
            hasNotification,
            isLegacy,
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
        ]}
      />
    );
  }
}
