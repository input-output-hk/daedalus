'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.generateSupportRequestLink = void 0;
const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};
const generateSupportRequestLink = (baseUrl, environmentData, locale) => {
  const {
    version,
    apiVersion,
    network,
    build,
    installerVersion,
    os,
    buildNumber,
  } = environmentData;
  const supportRequestData = {
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
      ([key, val]) => `${encodeURIComponent(key)}=${encodeURIComponent(val)}`
    )
    .join('&')}`;
};
exports.generateSupportRequestLink = generateSupportRequestLink;
//# sourceMappingURL=reporting.js.map
