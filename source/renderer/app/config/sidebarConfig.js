// @flow
import { ROUTES } from '../routes-config';
import walletsIcon from '../assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../assets/images/sidebar/settings-ic.inline.svg';
import paperWalletCertificateIcon from '../assets/images/sidebar/paper-certificate-ic.inline.svg';
import stakingIcon from '../assets/images/sidebar/delegation-ic.inline.svg';

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
  STAKING: {
    name: 'STAKING',
    route: ROUTES.STAKING.ROOT,
    icon: stakingIcon,
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
  CATEGORIES_BY_NAME.STAKING,
  // CATEGORIES_BY_NAME.STAKING_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
];

export const sidebarConfig = { CATEGORIES_BY_NAME, CATEGORIES };
