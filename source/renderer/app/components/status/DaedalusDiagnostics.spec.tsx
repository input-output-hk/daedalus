import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import type { ChainDamage } from '../../../../common/types/watchdog.types';
import DaedalusDiagnostics from './DaedalusDiagnostics';

// PopOver takes its skin from a theme provider; its trigger is enough here.
jest.mock('react-polymorph/lib/components/PopOver', () => ({
  PopOver: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

const defaultProps = {
  systemInfo: {
    platform: 'Linux',
    platformVersion: '1.0',
    cpu: 'Test CPU',
    ram: '16 GB',
    availableDiskSpace: '1,024 MB',
    hasMetHardwareRequirements: true,
    isRTSFlagsModeEnabled: false,
  },
  coreInfo: {
    daedalusVersion: '1.0.0',
    daedalusBuildNumber: '1',
    daedalusProcessID: '100',
    daedalusMainProcessID: '200',
    daedalusStateDirectoryPath: '/tmp/state',
    isBlankScreenFixActive: false,
    cardanoNodeVersion: '10.0.0',
    cardanoNodePID: 123,
    cardanoNodeUptime: '1 minute',
    cardanoWalletVersion: '2026.1.0',
    cardanoWalletPID: 456,
    cardanoWalletUptime: '1 minute',
    cardanoWalletRestartCount: 0,
    cardanoWalletApiPort: 8090,
    cardanoNetwork: 'mainnet',
  },
  cardanoNodeState: 'running' as const,
  isNodeResponding: true,
  isNodeSyncing: true,
  isNodeInSync: false,
  isNodeTimeCorrect: true,
  nodeConnectionError: null,
  isConnected: true,
  isSynced: false,
  syncPercentage: 62.5,
  localTimeDifference: 0,
  isSystemTimeCorrect: true,
  isSystemTimeIgnored: false,
  isCheckingSystemTime: false,
  isForceCheckingSystemTime: false,
  localTip: { epoch: 100, slot: 200 } as any,
  networkTip: { epoch: 101, slot: 300 } as any,
  onOpenStateDirectory: jest.fn(),
  onOpenExternalLink: jest.fn(),
  onRestartNode: { trigger: jest.fn() } as any,
  onRestartWallet: { trigger: jest.fn() } as any,
  onClose: jest.fn(),
  onCopyStateDirectoryPath: jest.fn(),
  onForceCheckNetworkClock: jest.fn(),
  isMithrilPartialSyncWorking: false,
  isMithrilSignificantlyBehind: false,
  isMithrilProbeFailed: false,
  isMithrilAtOrPastSnapshot: false,
  onStartMithrilPartialSync: jest.fn(() => Promise.resolve()),
  diagnosticsWallets: [{ id: 'wallet-a', name: 'Wallet A' }],
  defaultDiagnosticsWalletId: 'wallet-a',
  diagnosticsAvailable: true,
  diagnosticsReady: true,
  isDappLaunching: false,
  consoleCaptureAvailable: true,
  consoleCaptureEnabled: false,
  onConsoleCaptureChange: jest.fn(),
  onLaunchDapp: jest.fn(() => Promise.resolve()),
  chainDamage: null,
  isNodeHeld: false,
  onRebuildChain: jest.fn(),
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <DaedalusDiagnostics {...defaultProps} {...overrides} />
    </IntlProvider>
  );

describe('DaedalusDiagnostics', () => {
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(cleanup);

  it('retains ordinary diagnostics without dApp launch or console controls when disabled', () => {
    global.cip30Enabled = false;
    renderComponent();
    expect(screen.getByText('Test CPU')).toBeInTheDocument();
    expect(screen.queryByLabelText('DApp URL')).not.toBeInTheDocument();
    expect(
      screen.queryByRole('checkbox', {
        name: 'Capture dApp console warnings and errors',
      })
    ).not.toBeInTheDocument();
  });

  it('labels and launches an arbitrary URL as untrusted with the selected wallet', async () => {
    const onLaunchDapp = jest.fn(() => Promise.resolve());
    renderComponent({
      diagnosticsWallets: [
        { id: 'wallet-a', name: 'Wallet A' },
        { id: 'wallet-b', name: 'Wallet B' },
      ],
      onLaunchDapp,
    });

    expect(
      screen.getByText(
        'This opens an untrusted website. Daedalus does not audit or endorse it. Review every wallet request.'
      )
    ).toBeInTheDocument();
    fireEvent.change(screen.getByLabelText('DApp URL'), {
      target: { value: 'https://example.com/app?private=value' },
    });
    fireEvent.change(screen.getByLabelText('Wallet'), {
      target: { value: 'wallet-b' },
    });
    fireEvent.submit(
      screen
        .getByRole('button', { name: 'Launch untrusted dApp' })
        .closest('form') as HTMLFormElement
    );

    expect(onLaunchDapp).toHaveBeenCalledWith(
      'https://example.com/app?private=value',
      'wallet-b',
      'Untrusted dApp'
    );
  });

  it('requires an explicit diagnostics opt-in for dApp console capture', () => {
    const onConsoleCaptureChange = jest.fn();
    renderComponent({ onConsoleCaptureChange });

    fireEvent.click(
      screen.getByRole('checkbox', {
        name: 'Capture dApp console warnings and errors',
      })
    );

    expect(onConsoleCaptureChange).toHaveBeenCalledWith(true);
  });
});

const damage: ChainDamage = {
  kind: 'ledger_point_missing',
  slot: 200102375,
  lastGoodSlot: null,
  detectedAt: '2026-10-16T12:00:00.000Z',
};

const props = (chainDamage: ChainDamage | null) =>
  ({
    systemInfo: {
      platform: 'Linux',
      platformVersion: '6.6',
      cpu: 'cpu',
      ram: '16 GB',
      availableDiskSpace: '500 GB',
      hasMetHardwareRequirements: true,
      isRTSFlagsModeEnabled: false,
    },
    coreInfo: {
      daedalusVersion: '11.5.0',
      daedalusBuildNumber: '1',
      daedalusProcessID: '2',
      daedalusMainProcessID: '3',
      isBlankScreenFixActive: false,
      cardanoNodeVersion: '11.1.2',
      cardanoNodePID: 4,
      cardanoNodeUptime: '1s',
      cardanoWalletVersion: 'v1',
      cardanoWalletPID: 5,
      cardanoWalletUptime: '1s',
      cardanoWalletRestartCount: 0,
      cardanoWalletApiPort: 1,
      cardanoNetwork: 'mainnet',
      daedalusStateDirectoryPath: '/state',
    },
    cardanoNodeState: 'running',
    isNodeResponding: true,
    isNodeSyncing: false,
    isNodeInSync: true,
    isNodeTimeCorrect: true,
    nodeConnectionError: null,
    isConnected: true,
    isSynced: true,
    syncPercentage: 100,
    localTimeDifference: 0,
    isSystemTimeCorrect: true,
    isSystemTimeIgnored: false,
    isCheckingSystemTime: false,
    localTip: null,
    networkTip: null,
    chainDamage,
  }) as any;

// The copied text is built from the instance's props and intl context alone.
const diagnosticsText = (
  chainDamage: ChainDamage | null,
  overrides: Record<string, unknown> = {}
) => {
  const { intl } = new IntlProvider({
    locale: 'en-US',
    messages: translations,
  } as any).getChildContext();
  const diagnostics = new DaedalusDiagnostics({
    ...props(chainDamage),
    ...overrides,
  });
  diagnostics.context = { intl };
  return diagnostics.getDiagnosticsText();
};

describe('DaedalusDiagnostics copied text', () => {
  it('reports no damage in the Blockchain data row', () => {
    expect(diagnosticsText(null)).toContain(
      'Blockchain data: No damage detected'
    );
  });

  it('reports the last detection in the Blockchain data row', () => {
    const time = new Date(damage.detectedAt).toLocaleString('en-US');
    expect(diagnosticsText(damage)).toContain(
      `Blockchain data: Damage detected at slot 200102375 (${time})`
    );
  });
});

describe('DaedalusDiagnostics node state on the damaged data screen', () => {
  afterEach(cleanup);

  it('reports the node as stopped while the watchdog holds it, in the dialog and the copied text', () => {
    renderComponent({
      cardanoNodeState: 'chain-damaged',
      isNodeHeld: true,
      chainDamage: damage,
    });
    expect(screen.getByText('Stopped')).toBeInTheDocument();
    expect(screen.queryByText('Running')).toBeNull();
    expect(
      diagnosticsText(damage, {
        cardanoNodeState: 'chain-damaged',
        isNodeHeld: true,
      })
    ).toContain('Cardano node state: Stopped');
  });

  it('reports the node as running while it runs behind the screen', () => {
    expect(
      diagnosticsText(damage, {
        cardanoNodeState: 'chain-damaged',
        isNodeHeld: false,
      })
    ).toContain('Cardano node state: Running');
  });
});

describe('DaedalusDiagnostics node restart', () => {
  afterEach(cleanup);

  const renderDiagnostics = (overrides = {}) => {
    const onRestartNode = { trigger: jest.fn() };
    const view = (extra = {}) => (
      <IntlProvider locale="en-US" messages={translations}>
        <DaedalusDiagnostics
          {...props(damage)}
          onRestartNode={onRestartNode}
          onRestartWallet={{ trigger: jest.fn() }}
          onOpenStateDirectory={jest.fn()}
          onOpenExternalLink={jest.fn()}
          onClose={jest.fn()}
          onCopyStateDirectoryPath={jest.fn()}
          onForceCheckNetworkClock={jest.fn()}
          onStartMithrilPartialSync={jest.fn()}
          onRebuildChain={jest.fn()}
          isMithrilPartialSyncWorking={false}
          isMithrilSignificantlyBehind={false}
          isMithrilProbeFailed={false}
          isMithrilAtOrPastSnapshot={false}
          diagnosticsWallets={[]}
          defaultDiagnosticsWalletId=""
          diagnosticsAvailable={false}
          diagnosticsReady={false}
          isDappLaunching={false}
          consoleCaptureAvailable={false}
          consoleCaptureEnabled={false}
          onConsoleCaptureChange={jest.fn()}
          onLaunchDapp={jest.fn()}
          {...overrides}
          {...extra}
        />
      </IntlProvider>
    );
    const { rerender } = render(view());
    return {
      onRestartNode,
      rerender: (extra: Record<string, unknown>) => rerender(view(extra)),
    };
  };
  const restartButton = () =>
    screen.getByRole('button', { name: /Cardano node/ });

  it('offers the node restart while the watchdog holds the node', () => {
    const { onRestartNode } = renderDiagnostics({
      cardanoNodeState: 'chain-damaged',
      isNodeHeld: true,
    });
    expect(restartButton()).toBeEnabled();
    fireEvent.click(restartButton());
    expect(onRestartNode.trigger).toHaveBeenCalledTimes(1);
    expect(restartButton()).toHaveTextContent('Restarting Cardano node...');
  });

  it('offers no node restart on the damaged data screen while a node runs', () => {
    renderDiagnostics({ cardanoNodeState: 'chain-damaged', isNodeHeld: false });
    expect(restartButton()).toBeDisabled();
  });

  it('ends the restart of a held node when the check reports the damage again', () => {
    const { rerender } = renderDiagnostics({
      cardanoNodeState: 'chain-damaged',
      isNodeHeld: true,
    });
    fireEvent.click(restartButton());
    rerender({ chainDamage: damage });
    expect(restartButton()).toHaveTextContent('Restarting Cardano node...');
    rerender({
      chainDamage: { ...damage, detectedAt: '2026-10-16T12:05:00.000Z' },
    });
    expect(restartButton()).toHaveTextContent('Restart Cardano node');
    expect(restartButton()).toBeEnabled();
  });
});
