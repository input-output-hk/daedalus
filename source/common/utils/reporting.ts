import type { SupportRequests } from '../types/support-requests.types';
import type { Environment } from '../types/environment.types';

export const SUPPORT_URL = 'https://daedalus.support.se7enlabs.com/';
export const FEATURE_REQUEST_URL =
  'https://github.com/input-output-hk/daedalus/issues/new/choose';

const localesFillForm: Record<string, string> = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

export const getSupportUrl = (locale: string): string => {
  if (locale === 'en-US') return SUPPORT_URL;
  const localeCode = locale.replace('-', '_');
  const supportLanguage = localesFillForm[locale] ?? locale;
  return `${SUPPORT_URL}?locale=${localeCode}&supportLanguage=${encodeURIComponent(supportLanguage)}`;
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
