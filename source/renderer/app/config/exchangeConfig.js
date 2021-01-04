// @flow
import { ACTIVE_EXCHANGE_API } from '../config/walletsConfig';
import coingeckoConfig from './exchangeConfig.coingecko';
import nomicsConfig from './exchangeConfig.nomics';

const { id: activeApiId } = ACTIVE_EXCHANGE_API;

let config;
if (activeApiId === nomicsConfig.id) config = nomicsConfig;
else config = coingeckoConfig;

export default config;
