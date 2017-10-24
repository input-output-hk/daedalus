// @flow
import { ROUTES } from '../../routes-config';

// TODO: restructure CATEGORIES into an array:
// CATEGORIES = [
//   WALLETS: {
//     route: ROUTES.WALLETS.ROOT,
//     icon: <wallets-icon.svg>,
//   },
//
//   ADA_REDEMPTION: {
//     route: ROUTES.ADA_REDEMPTION,
//     icon: <ada-redemption-icon.svg>,
//   },
//
//   SETTINGS: {
//     route: ROUTES.SETTINGS.ROOT,
//     icon: <wallets-settings-icon.svg>,
//   },
// ];

export const CATEGORIES = {
  WALLETS: ROUTES.WALLETS.ROOT,
  ADA_REDEMPTION: ROUTES.ADA_REDEMPTION,
  SETTINGS: ROUTES.SETTINGS.ROOT,
};
