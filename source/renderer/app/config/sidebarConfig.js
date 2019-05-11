// @flow
import { ROUTES } from '../routes-config';
import walletsIcon from '../assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../assets/images/sidebar/settings-ic.inline.svg';
import paperWalletCertificateIcon from '../assets/images/sidebar/paper-certificate-ic.inline.svg';
import decentralisationNotificationIcon from '../assets/images/sidebar/decentralisation-ic.inline.svg';

export const CATEGORIES_BY_NAME = {
  WALLETS: {
    name: 'WALLETS',
    route: ROUTES.WALLETS.ROOT,
    icon: walletsIcon,
  },
  PAPER_WALLET_CREATE_CERTIFICATE: {
    name: 'PAPER_WALLET_CREATE_CERTIFICATE',
    route: ROUTES.PAPER_WALLET_CREATE_CERTIFICATE,
    icon: paperWalletCertificateIcon,
  },
  CARDANO_DECENTRALISATION_NOTIFICATION: {
    name: 'CARDANO_DECENTRALISATION_NOTIFICATION',
    route: ROUTES.CARDANO_DECENTRALISATION_NOTIFICATION,
    icon: decentralisationNotificationIcon,
  },
  SETTINGS: {
    name: 'SETTINGS',
    route: ROUTES.SETTINGS.ROOT,
    icon: settingsIcon,
  },
};

export const CATEGORIES = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.PAPER_WALLET_CREATE_CERTIFICATE,
  CATEGORIES_BY_NAME.CARDANO_DECENTRALISATION_NOTIFICATION,
  CATEGORIES_BY_NAME.SETTINGS,
];

export const sidebarConfig = { CATEGORIES_BY_NAME, CATEGORIES };
