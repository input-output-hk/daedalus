import { ROUTES } from '../routes-config';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/walle... Remove this comment to see the full error message
import walletsIcon from '../assets/images/sidebar/wallet-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/setti... Remove this comment to see the full error message
import settingsIcon from '../assets/images/sidebar/settings-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/paper... Remove this comment to see the full error message
import paperWalletCertificateIcon from '../assets/images/sidebar/paper-certificate-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/deleg... Remove this comment to see the full error message
import delegationIcon from '../assets/images/sidebar/delegation-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/deleg... Remove this comment to see the full error message
import delegationProgressIcon from '../assets/images/sidebar/delegation-progress-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/netwo... Remove this comment to see the full error message
import networkInfoLogo from '../assets/images/sidebar/network-info-logo-cardano-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../assets/images/sidebar/votin... Remove this comment to see the full error message
import votingIcon from '../assets/images/sidebar/voting-ic.inline.svg';

export type SidebarCategoryInfo = {
  name: string;
  icon: string;
  route: string;
};
export const CATEGORIES_BY_NAME = {
  WALLETS: {
    name: 'WALLETS',
    icon: walletsIcon,
    route: ROUTES.WALLETS.ROOT,
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
  VOTING: {
    name: 'VOTING',
    icon: votingIcon,
    route: ROUTES.VOTING.REGISTRATION,
  },
};
export const CATEGORIES_WITH_DELEGATION_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];
export const CATEGORIES_WITHOUT_DELEGATION_COUNTDOWN = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];
export const CATEGORIES_LIST = [
  CATEGORIES_BY_NAME.WALLETS,
  CATEGORIES_BY_NAME.STAKING_DELEGATION_COUNTDOWN,
  CATEGORIES_BY_NAME.STAKING,
  CATEGORIES_BY_NAME.VOTING,
  CATEGORIES_BY_NAME.SETTINGS,
  CATEGORIES_BY_NAME.NETWORK_INFO,
];
export const sidebarConfig = {
  CATEGORIES_BY_NAME,
  CATEGORIES_LIST,
};
