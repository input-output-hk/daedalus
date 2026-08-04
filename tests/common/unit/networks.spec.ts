import {
  getNetworkExplorerUri,
  getNetworkExplorerUrl,
  getNetworkExplorerUrlByType,
} from '../../../source/renderer/app/utils/network';
import {
  MAINNET,
  STAGING,
  TESTNET,
  DEVELOPMENT,
} from '../../../source/common/types/environment.types';

describe('Function getNetworkExplorerUri returns:', () => {
  it('the correct Url for TESTNET', () => {
    // getNetworkExplorerUri
    const result = getNetworkExplorerUri('testnet');
    expect(result).toBe('explorer.cardano-testnet.iohkdev.io');
  });
  it('the correct Url for MAINNET', () => {
    // getNetworkExplorerUri
    const result = getNetworkExplorerUri('mainnet');
    expect(result).toBe('explorer.cardano.org');
  });
  it('the correct Url for STAGING', () => {
    // getNetworkExplorerUri
    const result = getNetworkExplorerUri('staging');
    expect(result).toBe('explorer.staging.cardano.org');
  });
});
describe('Function getNetworkExplorerUri passing no arguments', () => {
  it('should return MAINNET_EXPLORER_URL', () => {
    // getNetworkExplorerUri
    const result = getNetworkExplorerUri('');
    expect(result).toBe('explorer.cardano.org');
  });
});
describe('Function getNetworkExplorerUrl returns:', () => {
  it.each([MAINNET, TESTNET, DEVELOPMENT, STAGING, 'preprod', 'selfnode'])(
    'an https url for %s',
    (network) => {
      expect(getNetworkExplorerUrl(network).startsWith('https://')).toBe(true);
    }
  );
  it('the staging explorer host over https', () => {
    expect(getNetworkExplorerUrl(STAGING)).toBe(
      'https://explorer.staging.cardano.org'
    );
  });
});
describe('Function getNetworkExplorerUrlByType returns:', () => {
  it('an https url for a network outside the localised set', () => {
    expect(getNetworkExplorerUrlByType('tx', 'abc', STAGING, 'en-US')).toBe(
      'https://explorer.staging.cardano.org/txabc'
    );
  });
});
