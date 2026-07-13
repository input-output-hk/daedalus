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
    daedalusProcessID: '100',
    daedalusMainProcessID: '200',
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
  localTip: { epoch: 100, slot: 200 } as any,
  networkTip: { epoch: 101, slot: 300 } as any,
  isMithrilPartialSyncWorking: false,
  isMithrilPartialSyncEnabled: true,
  isMithrilPartialSyncSignificantlyBehind: true,
  isMithrilPartialSyncProbeFailed: false,
  isMithrilPartialSyncAtOrPastSnapshot: false,
  isMithrilBootstrapActive: false,
  onStartMithrilPartialSync: jest.fn(),
  onOpenStateDirectory: jest.fn(),
  onOpenExternalLink: jest.fn(),
  onRestartNode: { trigger: jest.fn() } as any,
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

  it('renders the Mithril Sync button without any sync-% or untranslated copy', () => {
    // PopOver is mocked to render only its children, so the hover-only tooltip copy is intentionally absent from these assertions.
    renderComponent();

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();
    expect(screen.queryByText(/% synced/)).not.toBeInTheDocument();
    expect(screen.queryByText(/!!!/)).not.toBeInTheDocument();
  });

  it('keeps the Mithril Sync button visible without sync-% when fully synced', () => {
    renderComponent({ isSynced: true, syncPercentage: 100 });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();
    expect(
      screen.queryByText(/currently 100% synced/i)
    ).not.toBeInTheDocument();
  });

  it('keeps the CTA disabled while partial sync work is in flight', () => {
    renderComponent({ isMithrilPartialSyncWorking: true });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeDisabled();
    expect(
      screen.getByText('Unavailable while Mithril work is already active.')
    ).toBeInTheDocument();
  });

  it('re-arms the CTA once partial sync work reaches a terminal state', () => {
    renderComponent({ isMithrilPartialSyncWorking: false });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();
  });

  it('hides all partial sync UI when the kill switch is off', () => {
    renderComponent({ isMithrilPartialSyncEnabled: false });

    expect(screen.queryByRole('button', { name: 'Mithril Sync' })).toBeNull();
    expect(screen.queryByText(/Mithril Sync/)).toBeNull();
  });

  it('keeps the recommendation/CTA visible when enabled but not significantly behind', () => {
    const { rerender } = renderComponent({
      isMithrilPartialSyncEnabled: true,
      isMithrilPartialSyncSignificantlyBehind: true,
    });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();

    rerender(
      <IntlProvider locale="en-US" messages={translations}>
        <DaedalusDiagnostics
          {...defaultProps}
          isMithrilPartialSyncEnabled
          isMithrilPartialSyncSignificantlyBehind={false}
        />
      </IntlProvider>
    );

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeEnabled();
  });

  it('keeps the CTA disabled while bootstrap work is active', () => {
    renderComponent({ isMithrilBootstrapActive: true });

    expect(screen.getByRole('button', { name: 'Mithril Sync' })).toBeDisabled();
    expect(
      screen.getByText('Unavailable while Mithril work is already active.')
    ).toBeInTheDocument();
  });

  it('still wires the diagnostics partial sync button through the extracted section', () => {
    renderComponent();

    screen.getByRole('button', { name: 'Mithril Sync' }).click();

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();
  });

  it('computes the renderer node-tip epoch difference for the confirmation copy', () => {
    renderComponent({
      networkTip: { epoch: 500, slot: 0, absoluteSlotNumber: 0 } as any,
      localTip: { epoch: 497, slot: 0, absoluteSlotNumber: 0 } as any,
    });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();

    expect(
      screen.getByText(
        'Your node is about 3 epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
  });

  it('falls back to the unknown behind-ness line when a tip is missing', () => {
    renderComponent({ networkTip: null });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();

    expect(
      screen.getByText(
        'Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
  });

  it('shows the unknown behind-ness line when the tips share an epoch (no misleading floor-to-1)', () => {
    renderComponent({
      networkTip: { epoch: 100, slot: 0, absoluteSlotNumber: 0 } as any,
      localTip: { epoch: 100, slot: 0, absoluteSlotNumber: 0 } as any,
    });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();

    expect(
      screen.getByText(
        'Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
  });
});
