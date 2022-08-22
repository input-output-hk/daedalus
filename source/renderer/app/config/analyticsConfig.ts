import { Network } from '../../../common/types/environment.types';

export const ANALYTICS_API_ENDPOINT = 'https://mazurek.matomo.cloud/matomo.php';
export const DEV_MODE_SITE_MAP_ID = 5;
export const NETWORK_TO_ANALYTICS_SITE_ID_MAP: Record<Network, number> = {
  mainnet: 4,
  mainnet_flight: 4,
  testnet: 3,
  preprod: 3,
  preview: 3,
  staging: 5,
  shelley_qa: 5,
  alonzo_purple: 5,
  selfnode: 5,
  development: 5,
  vasil_dev: 5,
};
