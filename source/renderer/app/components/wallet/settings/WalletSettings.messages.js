// @flow
import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const getWalletSettingsMessages = (): { [string]: ReactIntlMessage } =>
  defineMessages({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description:
      'Label for the "Transaction assurance security level" dropdown.',
  },
  undelegateWalletHeader: {
    id: 'wallet.settings.undelegateWallet.header',
    defaultMessage: '!!!Undelegating your wallet',
    description: 'Undelegate wallet header on the wallet settings page.',
  },
  undelegateWalletWarning: {
    id: 'wallet.settings.undelegateWallet.warning',
    defaultMessage:
      '!!!If you are planning to stop using this wallet and remove all funds, you should first undelegate it to recover your 2 ada deposit. You will continue getting delegation rewards during the three Cardano epochs after undelegating your wallet.',
    description: 'Undelegate wallet warning explaining the consequences.',
  },
  undelegateWalletDisabledWarning: {
    id: 'wallet.settings.undelegateWallet.disabledWarning',
    defaultMessage:
      "!!!This wallet is synchronizing with the blockchain, so this wallet's delegation status is currently unknown, and undelegation is not possible.",
    description:
      'Undelegate wallet disabled warning explaining why it is disabled.',
  },
  delegateWalletHeader: {
    id: 'wallet.settings.delegateWallet.header',
    defaultMessage: '!!!Delegate your wallet',
    description: 'Delegate wallet header on the wallet settings page.',
  },
  delegateWalletWarning: {
    id: 'wallet.settings.delegateWallet.warning',
    defaultMessage:
      "!!!This wallet is not delegated. Please, delegate the stake from this wallet to earn rewards and support the Cardano network's security.",
    description: 'Delegate wallet warning.',
  },
  delegateWalletDisabledWarning: {
    id: 'wallet.settings.delegateWallet.disabledWarning',
    defaultMessage:
      "!!!This wallet is synchronizing with the blockchain, so this wallet's delegation status is currently unknown, and delegation is not possible.",
    description:
      'Delegate wallet disabled warning explaining why it is disabled.',
  },
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
  },
  passwordLabel: {
    id: 'wallet.settings.password',
    defaultMessage: '!!!Password',
    description: 'Label for the "Password" field.',
  },
  passwordLastUpdated: {
    id: 'wallet.settings.passwordLastUpdated',
    defaultMessage: '!!!Last updated',
    description: 'Last updated X time ago message.',
  },
  passwordNotSet: {
    id: 'wallet.settings.passwordNotSet',
    defaultMessage: "!!!You still don't have password",
    description: "You still don't have password set message.",
  },
  });

export default getWalletSettingsMessages;
