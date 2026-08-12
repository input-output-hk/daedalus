import https from 'https';

import {
  DEFAULT_EXPLORER_NETWORK_PATH,
  getNetworkExplorerPath,
  getNetworkExplorerUri,
  getNetworkExplorerUrl,
  getNetworkExplorerUrlByType,
} from '../../../source/renderer/app/utils/network';

// Real mainnet data, used by the network drift check below.
const MAINNET_TX_ID =
  'f064cf1c1255f9a696547516e0f61c32a1444a503722e91a5be815ad25a02276';
const MAINNET_ADDRESS =
  'addr1qxck7ajxutlp6y5xhmqx4gxwuw2erhz2klxrf58h8sfu99lp5g6yq0dqf6vlvxqxcwd5cvyuh75y4sxdtxjxrqcrx7hstmv5sc';

describe('Function getNetworkExplorerUri returns:', () => {
  it('the explorer host for TESTNET', () => {
    const result = getNetworkExplorerUri();
    expect(result).toBe('explorer.cardano.org');
  });
  it('the correct Url for MAINNET', () => {
    const result = getNetworkExplorerUri();
    expect(result).toBe('explorer.cardano.org');
  });
});
describe('Function getNetworkExplorerUrl returns:', () => {
  it('the https explorer Url', () => {
    expect(getNetworkExplorerUrl()).toBe('https://explorer.cardano.org');
  });
});
describe('Function getNetworkExplorerPath returns:', () => {
  it('the matching path for the networks the explorer serves', () => {
    expect(getNetworkExplorerPath('mainnet')).toBe('mainnet');
    expect(getNetworkExplorerPath('preprod')).toBe('preprod');
    expect(getNetworkExplorerPath('preview')).toBe('preview');
  });
  it('the default path for networks without an explorer', () => {
    for (const network of ['testnet', 'staging', 'development', 'selfnode']) {
      expect(getNetworkExplorerPath(network)).toBe(
        DEFAULT_EXPLORER_NETWORK_PATH
      );
    }
  });
  it('the default path when no network is provided', () => {
    expect(getNetworkExplorerPath('')).toBe(DEFAULT_EXPLORER_NETWORK_PATH);
    expect(getNetworkExplorerPath(undefined)).toBe(
      DEFAULT_EXPLORER_NETWORK_PATH
    );
    expect(DEFAULT_EXPLORER_NETWORK_PATH).toBe('mainnet');
  });
});
describe('Function getNetworkExplorerUrlByType returns:', () => {
  it('a network-scoped tx Url for MAINNET', () => {
    expect(getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, 'mainnet')).toBe(
      `https://explorer.cardano.org/mainnet/tx/${MAINNET_TX_ID}`
    );
  });
  it('a network-scoped address Url for MAINNET', () => {
    expect(
      getNetworkExplorerUrlByType('address', MAINNET_ADDRESS, 'mainnet')
    ).toBe(`https://explorer.cardano.org/mainnet/address/${MAINNET_ADDRESS}`);
  });
  it('a network-scoped tx Url for PREPROD', () => {
    expect(getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, 'preprod')).toBe(
      `https://explorer.cardano.org/preprod/tx/${MAINNET_TX_ID}`
    );
  });
  it('a network-scoped tx Url for PREVIEW', () => {
    expect(getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, 'preview')).toBe(
      `https://explorer.cardano.org/preview/tx/${MAINNET_TX_ID}`
    );
  });
  it('a mainnet-scoped Url for networks without an explorer', () => {
    for (const network of ['testnet', 'staging', 'development', 'selfnode']) {
      expect(getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, network)).toBe(
        `https://explorer.cardano.org/mainnet/tx/${MAINNET_TX_ID}`
      );
    }
  });
  it('a mainnet-scoped Url when no network is provided', () => {
    expect(getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, '')).toBe(
      `https://explorer.cardano.org/mainnet/tx/${MAINNET_TX_ID}`
    );
    expect(getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, undefined)).toBe(
      `https://explorer.cardano.org/mainnet/tx/${MAINNET_TX_ID}`
    );
  });
});

// Opt-in network check: `EXPLORER_DRIFT_CHECK=1 yarn test:jest networks`.
// It requests the URLs the wallet actually opens, with real mainnet data, so
// a layout change on explorer.cardano.org is detected instead of silently
// producing dead links. Skipped by default to keep the suite offline.
const describeDriftCheck = process.env.EXPLORER_DRIFT_CHECK
  ? describe
  : describe.skip;
describeDriftCheck('The explorer URLs the wallet opens:', () => {
  const expectReachable = (url: string) =>
    new Promise<void>((resolve, reject) => {
      https
        .get(url, (response) => {
          response.resume();
          const { statusCode, headers } = response;
          // follow a single redirect, the explorer normalises some paths
          if (
            statusCode &&
            statusCode >= 300 &&
            statusCode < 400 &&
            headers.location
          ) {
            resolve(expectReachable(new URL(headers.location, url).toString()));
            return;
          }
          try {
            expect({ url, statusCode }).toEqual({ url, statusCode: 200 });
            resolve();
          } catch (error) {
            reject(error);
          }
        })
        .on('error', reject);
    });

  it('resolve for a real mainnet transaction', async () => {
    await expectReachable(
      getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, 'mainnet')
    );
  }, 30000);
  it('resolve for a real mainnet address', async () => {
    await expectReachable(
      getNetworkExplorerUrlByType('address', MAINNET_ADDRESS, 'mainnet')
    );
  }, 30000);
  it('resolve for the preprod and preview explorers', async () => {
    await expectReachable(
      getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, 'preprod')
    );
    await expectReachable(
      getNetworkExplorerUrlByType('tx', MAINNET_TX_ID, 'preview')
    );
  }, 30000);
});
