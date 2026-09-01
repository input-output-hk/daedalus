import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../i18n/locales/en-US.json';
import DaedalusDiagnostics from './DaedalusDiagnostics';

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
  diagnosticsWallets: [{ id: 'wallet-a', name: 'Wallet A' }],
  defaultDiagnosticsWalletId: 'wallet-a',
  diagnosticsAvailable: true,
  diagnosticsReady: true,
  isDappLaunching: false,
  onLaunchDapp: jest.fn(() => Promise.resolve()),
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <DaedalusDiagnostics {...defaultProps} {...overrides} />
    </IntlProvider>
  );

describe('DaedalusDiagnostics', () => {
  afterEach(cleanup);


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
});
