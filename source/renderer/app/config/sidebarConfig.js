// @flow
import { ROUTES } from '../routes-config';
import walletsIcon from '../assets/images/sidebar/wallet-ic.inline.svg';
import hardwareWalletsIcon from '../assets/images/sidebar/hardware-wallet-ic.inline.svg';
import redeemItnRewardsIcon from '../assets/images/sidebar/redeem-itn-rewards-ic.inline.svg';
import settingsIcon from '../assets/images/sidebar/settings-ic.inline.svg';
import paperWalletCertificateIcon from '../assets/images/sidebar/paper-certificate-ic.inline.svg';
import delegationIcon from '../assets/images/sidebar/delegation-ic.inline.svg';
import delegationProgressIcon from '../assets/images/sidebar/delegation-progress-ic.inline.svg';
import networkInfoLogo from '../assets/images/sidebar/network-info-logo-cardano-ic.inline.svg';

export type SidebarCategoryInfo = {
  name: string,
  icon: string,
  route: string,
};

export const CATEGORIES_BY_NAME = {
  WALLETS: {
    name: 'WALLETS',
    icon: walletsIcon,
    route: ROUTES.WALLETS.ROOT,
  },
  HARDWARE_WALLETS: {
    name: 'HARDWARE_WALLETS',
    icon: hardwareWalletsIcon,
    route: ROUTES.HARDWARE_WALLETS.ROOT,
  },
  REDEEM_ITN_REWARDS: {
    name: 'REDEEM_ITN_REWARDS',
    icon: redeemItnRewardsIcon,
    route: ROUTES.REDEEM_ITN_REWARDS,
  },
  PAPER_WALLET_CREATE_CERTIFICATE: {
    name: 'PAPER_WALLET_CREATE_CERTIFICATE',
    icon: paperWalletCertificateIcon,
    route: ROUTES.PAPER_WALLET_CREATE_CERTIFICATE,
  },
  STAKING_DELEGATION_COUNTDOWN: {
    name: 'STAKING_DELEGATION_COUNTDOWN',
    icon: delegationIcon,
    route: ROUTES.STAKING.COUNTDOWN,
  },
  STAKING: {
    name: 'STAKING',
    icon: delegationProgressIcon,
    route: ROUTES.STAKING.ROOT,
  },
  SETTINGS: {
    name: 'SETTINGS',
    icon: settingsIcon,
    route: ROUTES.SETTINGS.ROOT,
  },
  NETWORK_INFO: {
    name: 'NETWORK_INFO',
    icon: networkInfoLogo,
    route: ROUTES.NETWORK_INFO,
  },
};

export const CATEGORIES = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.REDEEM_ITN_REWARDS,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

export const CATEGORIES_WITHOUT_NETWORK_INFO = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.REDEEM_ITN_REWARDS,
  CATEGORIES_BY_NAME.SETTINGS,
];

export const CATEGORIES_WITH_DELEGATION_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.REDEEM_ITN_REWARDS,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

export const CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.REDEEM_ITN_REWARDS,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

export const CATEGORIES_WITH_STAKING = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.REDEEM_ITN_REWARDS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];

export const CATEGORIES_WITH_HARDWARE_WALLETS = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.HARDWARE_WALLETS,
  CATEGORIES_BY_NAME.REDEEM_ITN_REWARDS,
  CATEGORIES_BY_NAME.SETTINGS,
];

export const sidebarConfig = {
  CATEGORIES_BY_NAME,
  CATEGORIES,
  CATEGORIES_WITHOUT_NETWORK_INFO,
  CATEGORIES_WITH_DELEGATION_COUNTDOWN,
  CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN,
  CATEGORIES_WITH_STAKING,
  CATEGORIES_WITH_HARDWARE_WALLETS,
};
