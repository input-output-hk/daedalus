import React from 'react';
import { cleanup, render } from '@testing-library/react';
import {
  DaedalusDiagnosticsDialog,
  shouldCloseDiagnosticsForPartialSyncOverlay,
} from './DaedalusDiagnosticsDialog';

jest.mock('react-modal', () => ({
  __esModule: true,
  default: ({ children }) => React.createElement('div', null, children),
}));

jest.mock('../../components/status/DaedalusDiagnostics', () => ({
  __esModule: true,
  default: () => React.createElement('div', null, 'Daedalus diagnostics'),
}));

const createBaseProps = () => ({
  actions: {
    app: {
      closeDaedalusDiagnosticsDialog: {
        trigger: jest.fn(),
      },
    },
    networkStatus: {
      forceCheckNetworkClock: { trigger: jest.fn() },
      copyStateDirectoryPath: { trigger: jest.fn() },
      restartNode: { trigger: jest.fn() },
    },
  },
  stores: {
    app: {
      openExternalLink: jest.fn(),
    },
    mithrilBootstrap: {
      status: 'idle',
    },
    mithrilPartialSync: {
      status: 'idle',
      isActive: false,
      startPartialSync: jest.fn(),
    },
    networkStatus: {
      cardanoNodeState: 'running',
      isNodeResponding: true,
      isNodeSyncing: true,
      isNodeInSync: false,
      isNodeTimeCorrect: true,
      isConnected: true,
      isSynced: false,
      syncPercentage: 62.5,
      hasBeenConnected: true,
      localTimeDifference: 0,
      isSystemTimeCorrect: true,
      isSystemTimeIgnored: false,
      openStateDirectory: jest.fn(),
      getNetworkInfoRequest: { error: null },
      networkTip: { epoch: 1, slot: 2 },
      localTip: { epoch: 1, slot: 1 },
      environment: {
        network: 'mainnet',
        version: '1.0.0',
        rendererProcessID: 1,
        mainProcessID: 2,
        isBlankScreenFixActive: false,
        nodeVersion: '10.0.0',
        apiVersion: '2026.1.0',
        build: '1',
        isDev: false,
        isMainnet: true,
        isStaging: false,
        isTestnet: false,
      },
      tlsConfig: { port: 8090 },
      cardanoNodePID: 11,
      cardanoWalletPID: 22,
      stateDirectoryPath: '/tmp/state',
      getNetworkClockRequest: {
        result: {},
        isExecuting: false,
        isExecutingWithArgs: jest.fn(() => false),
      },
    },
  },
  children: null,
  onClose: jest.fn(),
});

const withStatus = (props, status) => ({
  ...props,
  stores: {
    ...props.stores,
    mithrilPartialSync: {
      ...props.stores.mithrilPartialSync,
      status,
      isActive: status !== 'idle',
    },
  },
});

describe('shouldCloseDiagnosticsForPartialSyncOverlay', () => {
  it('closes only when partial sync reaches an overlay-backed status', () => {
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('stopping-node', 'preparing')
    ).toBe(true);
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('preparing', 'downloading')
    ).toBe(false);
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('idle', 'failed')
    ).toBe(true);
    expect(
      shouldCloseDiagnosticsForPartialSyncOverlay('stopping-node', 'idle')
    ).toBe(false);
  });
});

describe('DaedalusDiagnosticsDialog', () => {
  afterEach(cleanup);

  it('closes the dialog only when partial sync transitions into an overlay-backed backend status', () => {
    const baseProps = createBaseProps();
    const { rerender } = render(
      React.createElement(
        DaedalusDiagnosticsDialog,
        withStatus(baseProps, 'stopping-node') as any
      )
    );

    rerender(
      React.createElement(DaedalusDiagnosticsDialog, withStatus(baseProps, 'idle') as any)
    );

    expect(
      baseProps.actions.app.closeDaedalusDiagnosticsDialog.trigger
    ).not.toHaveBeenCalled();

    rerender(
      React.createElement(
        DaedalusDiagnosticsDialog,
        withStatus(baseProps, 'preparing') as any
      )
    );

    expect(
      baseProps.actions.app.closeDaedalusDiagnosticsDialog.trigger
    ).toHaveBeenCalledTimes(1);
  });

  it('also closes when a terminal overlay-backed status arrives directly from idle', () => {
    const baseProps = createBaseProps();
    const { rerender } = render(
      React.createElement(DaedalusDiagnosticsDialog, withStatus(baseProps, 'idle') as any)
    );

    rerender(
      React.createElement(DaedalusDiagnosticsDialog, withStatus(baseProps, 'failed') as any)
    );

    expect(
      baseProps.actions.app.closeDaedalusDiagnosticsDialog.trigger
    ).toHaveBeenCalledTimes(1);
  });
});
