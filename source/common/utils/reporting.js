// @flow

import { getLocale } from '../../main/utils/getLocale';
import { environment } from '../../main/environment';
import serialize from '../../renderer/app/utils/serialize';
import type { SupportRequests } from '../types/support-requests.types';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

export const generateSupportRequestLink = (baseUrl: string): string => {
  const {
    version,
    apiVersion,
    network,
    build,
    installerVersion,
    os,
    buildNumber,
  } = environment;

  const networkLocale = getLocale(network);

  const supportRequestData: SupportRequests = {
    frontendVersion: version,
    backendVersion: apiVersion,
    network: network === 'development' ? 'staging' : network,
    build,
    installerVersion,
    os,
    networkLocale,
    product: `Daedalus wallet - ${network}`,
    supportLanguage: localesFillForm[networkLocale],
    productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
  };

  return `${baseUrl}?${serialize(supportRequestData)}`;
};
