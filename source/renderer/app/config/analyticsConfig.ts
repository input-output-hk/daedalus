import { Network } from '../../../common/types/environment.types';

export const ANALYTICS_API_ENDPOINT = 'https://matomo.cw.iog.io/matomo.php';
export const PRIVACY_POLICY_LINK =
  'https://static.iohk.io/terms/iog-privacy-policy.pdf';

// ID used when Daedalus is launched from nix-shell
export const DEV_MODE_SITE_MAP_ID = 11;

// IDs used when Daedalus is launched as a binary (installed with installer)
export const NETWORK_TO_ANALYTICS_SITE_ID_MAP: Record<Network, number> = {
  mainnet: 2,
  mainnet_flight: 12,
  testnet: 3,
  preprod: 9,
  preview: 10,
  staging: 4,
  shelley_qa: 6,
  alonzo_purple: 7,
  selfnode: 11,
  development: 11,
  vasil_dev: 8,
};

export const CPU_DIMENSION_KEY = 'dimension1';
export const RAM_DIMENSION_KEY = 'dimension2';
export const OS_DIMENSION_KEY = 'dimension3';
export const VERSION_DIMENSION_KEY = 'dimension4';
export const USES_LEGACY_WALLET_DIMENSION_KEY = 'dimension5';
export const USES_HARDWARE_WALLET_DIMENSION_KEY = 'dimension6';
