import React from 'react';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import { DaedalusDiagnosticsDialog } from './DaedalusDiagnosticsDialog';

// PopOver takes its skin from a theme provider; its trigger is enough here.
jest.mock('react-polymorph/lib/components/PopOver', () => ({
  PopOver: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

const Dialog = (DaedalusDiagnosticsDialog as any).wrappedComponent;

const renderDialog = () => {
  const environment = {
    network: 'mainnet',
    version: '11.5.0',
    build: '1',
    rendererProcessID: 2,
    mainProcessID: 3,
    isBlankScreenFixActive: false,
    nodeVersion: '11.1.3',
    apiVersion: 'v2026-09-16',
    os: 'Linux',
    platformVersion: '6.6',
    cpu: [{ model: 'cpu' }],
    ram: 16 * 1024 ** 3,
    hasMetHardwareRequirements: true,
    isDev: false,
    isMainnet: true,
    isStaging: false,
    isTestnet: false,
  };
  const stores = {
    app: { openExternalLink: jest.fn() },
    networkStatus: {
      isNodeResponding: true,
      isNodeSyncing: true,
      isNodeInSync: true,
      isNodeTimeCorrect: true,
      isConnected: true,
      isSynced: true,
      syncPercentage: 100,
      hasBeenConnected: true,
      localTimeDifference: 0,
      isSystemTimeCorrect: true,
      isSystemTimeIgnored: false,
      openStateDirectory: jest.fn(),
      getNetworkInfoRequest: { error: null },
      networkTip: null,
      localTip: null,
      environment,
      stateDirectoryPath: '/state',
      getNetworkClockRequest: {
        result: {},
        isExecuting: false,
        isExecutingWithArgs: () => false,
      },
      diskSpaceAvailable: '500 GB',
      isRTSFlagsModeEnabled: false,
    },
    backend: {
      nodePid: 4,
      walletPid: 5,
      watchdogPid: 6,
      nodeStartedAt: null,
      walletStartedAt: null,
      walletRestartCount: 0,
      walletPort: 8090,
      nodeForceKilled: false,
      lastWalletExitCode: null,
      nodeSocketWaitMs: null,
      walletReadyWaitMs: null,
      loadingPhase: 'ready',
      mithrilPhase: null,
      mithrilSignificantlyBehind: null,
      isStopping: false,
      lastChainDamage: null,
      nodeHeld: false,
      rebuildChain: jest.fn(),
      startMithrilForce: jest.fn(),
    },
    dapp: {
      diagnosticsAvailable: false,
      diagnosticsReady: false,
      isLaunching: false,
      consoleCaptureAvailable: false,
      consoleCaptureEnabled: false,
      launchDiagnostics: jest.fn(),
      setConsoleCaptureEnabled: jest.fn(),
    },
    wallets: { eligibleDappWallets: [], activeDappWallet: null },
  };
  const actions = {
    app: { closeDaedalusDiagnosticsDialog: { trigger: jest.fn() } },
    networkStatus: {
      forceCheckNetworkClock: { trigger: jest.fn() },
      restartNode: { trigger: jest.fn() },
      restartWallet: { trigger: jest.fn() },
      copyStateDirectoryPath: { trigger: jest.fn() },
    },
  };
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <Dialog stores={stores} actions={actions} />
    </IntlProvider>
  );
  return {
    backend: stores.backend,
    close: actions.app.closeDaedalusDiagnosticsDialog.trigger,
  };
};

const sentBefore = (first: jest.Mock, second: jest.Mock) =>
  first.mock.invocationCallOrder[0] < second.mock.invocationCallOrder[0];

describe('DaedalusDiagnosticsDialog', () => {
  afterEach(cleanup);

  it('closes once a confirmed rebuild is sent', () => {
    const { backend, close } = renderDialog();
    fireEvent.click(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    );
    expect(close).not.toHaveBeenCalled();
    fireEvent.click(screen.getByRole('button', { name: 'Rebuild' }));
    expect(backend.rebuildChain).toHaveBeenCalledTimes(1);
    expect(close).toHaveBeenCalledTimes(1);
    expect(sentBefore(backend.rebuildChain, close)).toBe(true);
  });

  it('stays open when the rebuild confirmation is left with Back', () => {
    const { backend, close } = renderDialog();
    fireEvent.click(
      screen.getByRole('button', { name: 'Rebuild blockchain data' })
    );
    fireEvent.click(screen.getByRole('button', { name: 'Back' }));
    expect(backend.rebuildChain).not.toHaveBeenCalled();
    expect(close).not.toHaveBeenCalled();
  });

  it('closes once a confirmed Mithril sync is sent', async () => {
    const { backend, close } = renderDialog();
    fireEvent.click(screen.getByRole('button', { name: 'Mithril Sync' }));
    expect(close).not.toHaveBeenCalled();
    await act(async () => {
      fireEvent.click(
        screen.getByRole('button', { name: 'Start Mithril Sync' })
      );
    });
    expect(backend.startMithrilForce).toHaveBeenCalledTimes(1);
    expect(close).toHaveBeenCalledTimes(1);
    expect(sentBefore(backend.startMithrilForce, close)).toBe(true);
  });

  it('stays open when the Mithril sync confirmation is left', () => {
    const { backend, close } = renderDialog();
    fireEvent.click(screen.getByRole('button', { name: 'Mithril Sync' }));
    fireEvent.click(
      screen.getByRole('button', { name: 'Back to Daedalus Diagnostics' })
    );
    expect(backend.startMithrilForce).not.toHaveBeenCalled();
    expect(close).not.toHaveBeenCalled();
  });
});
