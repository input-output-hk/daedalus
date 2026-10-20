/** @jest-environment node */
import type { BrowserWindow } from 'electron';
import {
  DAPP_BROWSER_OPEN_CHANNEL,
  DAPP_BROWSER_CLOSE_CHANNEL,
  DAPP_BROWSER_STATUS_CHANNEL,
  DAPP_CONNECTIONS_CHANNEL,
  DAPP_COLLATERAL_CHANNEL,
  WALLET_INPUT_SELECTION_CHANNEL,
  WALLET_TRANSACTION_APPROVAL_CHANNEL,
} from '../../common/ipc/api';
import { DappBrowserManager } from '../dapp/DappBrowserManager';
import { DappRouteLeaseService } from '../dapp/DappRouteLease';
import {
  getCollateralService,
  getDappConnectionService,
} from '../cip30/Cip30Broker';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  handleDappBrowserRequests,
  closeDappBrowser,
  getCurrentDappRouteLease,
} from './dappBrowser';
import { handleDappCollateralRequests } from './collateral';
import { handleDappConnectionRequests } from './dappConnections';
import { handleNativeTransactionApprovalRequests } from './nativeTransactionApproval';
import {
  handleWalletApprovalRequests,
  consentCoordinator,
} from './walletApproval';

jest.mock('../config', () => ({
  dappRuntimeConfig: {
    cip30Enabled: false,
    dappSandboxPackageCluster: 'mainnet',
    dappNetwork: { cluster: 'mainnet', genesisHash: '11'.repeat(32) },
  },
  dappLaunchPolicy: { allows: jest.fn(() => false) },
  isFlight: false,
}));
jest.mock('../environment', () => ({ environment: { isDev: false } }));
jest.mock('../dapp/DappBrowserManager', () => ({
  DappBrowserManager: jest.fn(),
}));
jest.mock('../dapp/DappRouteLease', () => ({
  DappRouteLeaseService: jest.fn(),
}));
jest.mock('../cip30/Cip30Broker', () => ({
  getCollateralService: jest.fn(),
  getDappConnectionService: jest.fn(),
}));
jest.mock('../cip30/ConsentCoordinator', () => ({
  ConsentCoordinator: jest.fn(() => ({
    request: jest.fn(),
    activity: jest.fn(),
  })),
}));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation((name: string) => ({
    name,
    onRequest: jest.fn(),
  })),
}));
jest.mock('../utils/logging', () => ({ logger: { error: jest.fn() } }));

// Channels are constructed during module loading, before Jest clears mock calls.
const channels = (MainIpcChannel as unknown as jest.Mock).mock.results.map(
  ({ value }) => value
);

describe('nonpilot main activation', () => {
  it('registers prompt unavailable responses without starting guests, route tracking, persistence or approval', async () => {
    const on = jest.fn();
    const window = { webContents: { on } } as unknown as BrowserWindow;
    handleDappBrowserRequests(window);
    handleDappCollateralRequests();
    handleDappConnectionRequests();
    handleNativeTransactionApprovalRequests();
    handleWalletApprovalRequests(window);

    for (const name of [
      DAPP_BROWSER_OPEN_CHANNEL,
      DAPP_BROWSER_CLOSE_CHANNEL,
      DAPP_BROWSER_STATUS_CHANNEL,
      DAPP_CONNECTIONS_CHANNEL,
      DAPP_COLLATERAL_CHANNEL,
      WALLET_INPUT_SELECTION_CHANNEL,
      WALLET_TRANSACTION_APPROVAL_CHANNEL,
    ]) {
      const channel = channels.find((candidate) => candidate.name === name);
      expect(channel.onRequest).toHaveBeenCalledTimes(1);
      const handler = channel.onRequest.mock.calls[0][0];
      await expect(handler({})).rejects.toThrow('CIP-30 is unavailable');
    }
    await expect(closeDappBrowser()).resolves.toBeUndefined();
    expect(getCurrentDappRouteLease()).toBeNull();
    expect(DappBrowserManager).not.toHaveBeenCalled();
    expect(DappRouteLeaseService).not.toHaveBeenCalled();
    expect(getCollateralService).not.toHaveBeenCalled();
    expect(getDappConnectionService).not.toHaveBeenCalled();
    expect(consentCoordinator.request).not.toHaveBeenCalled();
    expect(on).not.toHaveBeenCalled();
  });
});
