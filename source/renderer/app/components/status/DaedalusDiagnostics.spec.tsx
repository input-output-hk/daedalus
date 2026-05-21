import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
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
    daedalusProcessID: 100,
    daedalusMainProcessID: 200,
    daedalusStateDirectoryPath: '/tmp/state',
    isBlankScreenFixActive: false,
    cardanoNodeVersion: '10.0.0',
    cardanoNodePID: 123,
    cardanoWalletVersion: '2026.1.0',
    cardanoWalletPID: 456,
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
  localTip: ({ epoch: 100, slot: 200 } as any),
  networkTip: ({ epoch: 101, slot: 300 } as any),
  isMithrilPartialSyncActive: false,
  isMithrilBootstrapActive: false,
  onStartMithrilPartialSync: jest.fn(),
  onOpenStateDirectory: jest.fn(),
  onOpenExternalLink: jest.fn(),
  onRestartNode: ({ trigger: jest.fn() } as any),
  onClose: jest.fn(),
  onCopyStateDirectoryPath: jest.fn(),
  onForceCheckNetworkClock: jest.fn(),
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <DaedalusDiagnostics {...defaultProps} {...overrides} />
    </IntlProvider>
  );

describe('DaedalusDiagnostics', () => {
  afterEach(cleanup);

  it('renders the Mithril partial sync recommendation with current sync context', () => {
    renderComponent();

    expect(
      screen.getByText(
        'Cardano node is currently 62.50% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Review what will happen before Daedalus starts Mithril partial sync.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Mithril Partial Sync' })
    ).toBeEnabled();
    expect(screen.queryByText(/!!!/)).not.toBeInTheDocument();
  });

  it('opens confirmation first and does not start partial sync before confirm', () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril partial sync begins',
      })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'Daedalus will stop Cardano node automatically, then download and restore verified Mithril data.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'If Mithril partial sync succeeds, Daedalus will restart Cardano node automatically and normal syncing will resume.'
      )
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'If the attempt fails, Daedalus can offer retry partial sync, restart normally on the current database, or wipe chain data and do a full Mithril sync.'
      )
    ).toBeInTheDocument();
    expect(onStartMithrilPartialSync).not.toHaveBeenCalled();
    expect(screen.queryByText(/!!!/)).not.toBeInTheDocument();
  });

  it('returns to diagnostics when confirmation is cancelled', () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    screen.getByRole('button', { name: 'Back to diagnostics' }).click();

    expect(
      screen.getByText(
        'Cardano node is currently 62.50% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.'
      )
    ).toBeInTheDocument();
    expect(onStartMithrilPartialSync).not.toHaveBeenCalled();
  });

  it('starts partial sync only after confirmation', () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    screen.getByRole('button', { name: 'Start Mithril partial sync' }).click();

    expect(onStartMithrilPartialSync).toHaveBeenCalledTimes(1);
  });

  it('renders the synced recommendation variant without the percentage text', () => {
    renderComponent({ isSynced: true, syncPercentage: 100 });

    expect(
      screen.getByText(
        'If Cardano node catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.'
      )
    ).toBeInTheDocument();
    expect(
      screen.queryByText(/currently 100% synced/i)
    ).not.toBeInTheDocument();
  });

  it('keeps the CTA disabled while partial sync is active', () => {
    renderComponent({ isMithrilPartialSyncActive: true });

    expect(
      screen.getByRole('button', { name: 'Mithril Partial Sync' })
    ).toBeDisabled();
    expect(
      screen.getByText('Unavailable while Mithril work is already active.')
    ).toBeInTheDocument();
  });

  it('keeps the CTA disabled while bootstrap work is active', () => {
    renderComponent({ isMithrilBootstrapActive: true });

    expect(
      screen.getByRole('button', { name: 'Mithril Partial Sync' })
    ).toBeDisabled();
    expect(
      screen.getByText('Unavailable while Mithril work is already active.')
    ).toBeInTheDocument();
  });
});
