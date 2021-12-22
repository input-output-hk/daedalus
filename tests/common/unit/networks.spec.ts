import { getNetworkExplorerUri } from "../../../source/renderer/app/utils/network";

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