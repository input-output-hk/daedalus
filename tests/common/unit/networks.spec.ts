import {
  getNetworkExplorerUri,
  getNetworkExplorerUrlByType,
} from '../../../source/renderer/app/utils/network';

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
describe('Function getNetworkExplorerUrlByType returns:', () => {
  const txId =
    'f064cf1c1255f9a696547516e0f61c32a1444a503722e91a5be815ad25a02276';
  const address = 'addr1q9zvfnrhaqm3lzs5vsvrsvhpg8f5hqmv5x8a55l6rxvq2xzq0k6a';

  it('a network-scoped tx Url for MAINNET', () => {
    expect(getNetworkExplorerUrlByType('tx', txId, 'mainnet', 'en-US')).toBe(
      `https://explorer.cardano.org/mainnet/tx/${txId}`
    );
  });
  it('a network-scoped address Url for MAINNET', () => {
    expect(
      getNetworkExplorerUrlByType('address', address, 'mainnet', 'en-US')
    ).toBe(`https://explorer.cardano.org/mainnet/address/${address}`);
  });
  it('a network-scoped tx Url for PREPROD', () => {
    expect(getNetworkExplorerUrlByType('tx', txId, 'preprod', 'en-US')).toBe(
      `https://explorer.cardano.org/preprod/tx/${txId}`
    );
  });
  it('a network-scoped tx Url for PREVIEW', () => {
    expect(getNetworkExplorerUrlByType('tx', txId, 'preview', 'en-US')).toBe(
      `https://explorer.cardano.org/preview/tx/${txId}`
    );
  });
  it('the legacy query-string Url for TESTNET', () => {
    expect(getNetworkExplorerUrlByType('tx', txId, 'testnet', 'en-US')).toBe(
      `https://explorer.cardano-testnet.iohkdev.io/en/transaction?id=${txId}`
    );
    expect(
      getNetworkExplorerUrlByType('address', address, 'testnet', 'ja-JP')
    ).toBe(
      `https://explorer.cardano-testnet.iohkdev.io/ja/address.html?address=${address}`
    );
  });
  it('the unprefixed Url for STAGING', () => {
    expect(getNetworkExplorerUrlByType('tx', txId, 'staging', 'en-US')).toBe(
      `http://explorer.staging.cardano.org/tx/${txId}`
    );
  });
  it('a mainnet-scoped Url for unknown networks', () => {
    expect(getNetworkExplorerUrlByType('tx', txId, '', 'en-US')).toBe(
      `https://explorer.cardano.org/mainnet/tx/${txId}`
    );
  });
});
