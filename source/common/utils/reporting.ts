import type { SupportRequests } from '../types/support-requests.types';
import type { Environment } from '../types/environment.types';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};
export const generateSupportRequestLink = (
  baseUrl: string,
  environmentData: Environment,
  locale: string
): string => {
  const {
    version,
    apiVersion,
    network,
    build,
    installerVersion,
    os,
    buildNumber,
  } = environmentData;
  const supportRequestData: SupportRequests = {
    frontendVersion: version,
    backendVersion: apiVersion,
    network: network === 'development' ? 'staging' : network,
    build,
    installerVersion,
    os,
    locale,
    product: `Daedalus wallet - ${network}`,
    supportLanguage: localesFillForm[locale],
    productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
  };
  return `${baseUrl}?${Object.entries(supportRequestData)
    .map(
      ([key, val]: [string, any]) =>
        `${encodeURIComponent(key)}=${encodeURIComponent(val)}`
    )
    .join('&')}`;
};
