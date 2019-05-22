// @flow
import { ROUTES } from '../routes-config';
import walletsIcon from '../assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../assets/images/sidebar/settings-ic.inline.svg';
import paperWalletCertificateIcon from '../assets/images/sidebar/paper-certificate-ic.inline.svg';
import delegationIcon from '../assets/images/sidebar/delegation-ic.inline.svg';
import delegationProgressIcon from '../assets/images/sidebar/delegation-progress-ic.inline.svg';

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
  STAKING_WITH_DELEGATION_COUNT_DOWN: {
    name: 'STAKING_WITH_DELEGATION_COUNT_DOWN',
    route: ROUTES.STAKING.DELEGATION_COUNTDOWN,
    icon: delegationIcon,
  },
  STAKING_WITHOUT_DELEGATION_COUNT_DOWN: {
    name: 'STAKING_WITHOUT_DELEGATION_COUNT_DOWN',
    route: ROUTES.STAKING.DELEGATION_CENTER,
    icon: delegationProgressIcon,
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
  CATEGORIES_BY_NAME.SETTINGS,
];

export const sidebarConfig = { CATEGORIES_BY_NAME, CATEGORIES };
