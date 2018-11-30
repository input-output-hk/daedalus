// @flow
import environment from '../../../common/environment';
import serialize from './serialize';
import { SUPPORT_URL_EN, SUPPORT_URL_JP } from '../config/urlsConfig';

export default (locale: string) => {
  const { version, os, API_VERSION, NETWORK, build, getInstallerVersion } = environment;
  const network = NETWORK === 'development' ? 'staging' : NETWORK;
  const info = {
    product: 'Daedalus Wallet',
    frontendVersion: version,
    backendVersion: API_VERSION,
    network,
    build,
    installerVersion: getInstallerVersion(),
    os,
    locale,
  };
  const SUPPORT_URL = locale === 'en' ? SUPPORT_URL_EN : SUPPORT_URL_JP;
  return `${SUPPORT_URL}?${serialize(info)}`;
};
