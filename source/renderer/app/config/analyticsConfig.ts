import { Network } from '../../../common/types/environment.types';

export const ANALYTICS_API_ENDPOINT =
  'https://daedalusqa2.matomo.cloud/matomo.php';
export const PRIVACY_POLICY_LINK =
  'https://static.iohk.io/terms/iog-privacy-policy.pdf';
export const DEV_MODE_SITE_MAP_ID = 1;
export const NETWORK_TO_ANALYTICS_SITE_ID_MAP: Record<Network, number> = {
  mainnet: 4,
  mainnet_flight: 4,
  testnet: 2,
  staging: 1,
  shelley_qa: 1,
  alonzo_purple: 1,
  selfnode: 1,
  development: 1,
};

export const CPU_DIMENSION_KEY = 'dimension2';
export const RAM_DIMENSION_KEY = 'dimension3';
export const OS_DIMENSION_KEY = 'dimension4';
export const VERSION_DIMENSION_KEY = 'dimension5';
