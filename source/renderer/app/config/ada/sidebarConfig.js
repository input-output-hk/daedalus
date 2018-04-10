// @flow
import { ROUTES } from '../../routes-config';
import walletsIcon from '../../assets/images/sidebar/wallet-ic.inline.svg';
import settingsIcon from '../../assets/images/sidebar/settings-ic.inline.svg';
import adaRedemptionIcon from '../../assets/images/sidebar/ada-redemption-ic.inline.svg';
import paperWalletCertificateIcon from '../../assets/images/sidebar/paper-certificate-ic.inline.svg';

export const CATEGORIES = [
  {
    name: 'WALLETS',
    route: ROUTES.WALLETS.ROOT,
    icon: walletsIcon,
  },
  {
    name: 'ADA_REDEMPTION',
    route: ROUTES.ADA_REDEMPTION,
    icon: adaRedemptionIcon,
  },
  {
    name: 'PAPER_WALLET_CREATE_CERTIFICATE',
    route: ROUTES.PAPER_WALLET_CREATE_CERTIFICATE,
    icon: paperWalletCertificateIcon,
  },
  {
    name: 'SETTINGS',
    route: ROUTES.SETTINGS.ROOT,
    icon: settingsIcon,
  },
];
