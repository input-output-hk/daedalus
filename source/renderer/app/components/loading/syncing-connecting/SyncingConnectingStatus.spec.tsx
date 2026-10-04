import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

import translations from '../../../i18n/locales/en-US.json';
import SyncingConnectingStatus, {
  formatStopElapsed,
} from './SyncingConnectingStatus';

const defaultProps = {
  cardanoNodeState: 'ready',
  blockSyncProgress: {
    replayedBlock: 0,
    validatingChunk: 0,
    pushingLedger: 0,
  },
  hasLoadedCurrentLocale: true,
  hasBeenConnected: true,
  isTlsCertInvalid: false,
  isConnected: false,
  isNodeStopping: false,
  isNodeStopped: false,
  isVerifyingBlockchain: false,
  nodeStartupPhase: null,
  backendStopProgress: null,
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <SyncingConnectingStatus {...defaultProps} {...overrides} />
    </IntlProvider>
  );

const quitting = {
  cardanoNodeState: 'stopping',
  isNodeStopping: true,
};

describe('SyncingConnectingStatus', () => {
  afterEach(cleanup);

  it('shows the reconnecting message when the wallet connection drops outside a quit', () => {
    renderComponent();
    expect(
      screen.getByText('Network connection lost - reconnecting')
    ).toBeInTheDocument();
  });

  it('shows the stopping message instead of reconnecting while Daedalus quits', () => {
    renderComponent(quitting);
    expect(screen.getByText('Stopping Cardano node')).toBeInTheDocument();
    expect(
      screen.queryByText('Network connection lost - reconnecting')
    ).not.toBeInTheDocument();
  });

  it('shows the stopping message while the wallet still answers during a quit', () => {
    renderComponent({ ...quitting, isConnected: true });
    expect(screen.getByText('Stopping Cardano node')).toBeInTheDocument();
    expect(screen.queryByText('Loading wallet data')).not.toBeInTheDocument();
  });

  it('shows the wallet step with its elapsed time', () => {
    renderComponent({
      ...quitting,
      backendStopProgress: {
        stage: 'stopping_wallet',
        elapsedMs: 4000,
        timeoutMs: 10_000,
      },
    });
    expect(
      screen.getByText('Stopping Cardano wallet (0:04)')
    ).toBeInTheDocument();
  });

  it('shows the node step with its elapsed time', () => {
    renderComponent({
      ...quitting,
      backendStopProgress: {
        stage: 'stopping_node',
        elapsedMs: 83_000,
        timeoutMs: 300_000,
      },
    });
    expect(
      screen.getByText('Waiting for Cardano node to close its database (1:23)')
    ).toBeInTheDocument();
  });

  it('names the wallet while a requested wallet restart runs', () => {
    renderComponent({ ...quitting, isRestartingWallet: true });
    expect(screen.getByText('Restarting Cardano wallet')).toBeInTheDocument();
    expect(
      screen.queryByText('Network connection lost - reconnecting')
    ).not.toBeInTheDocument();
  });

  it('shows no step before the watchdog reports progress', () => {
    renderComponent(quitting);
    expect(screen.queryByText(/Stopping Cardano wallet/)).toBeNull();
    expect(screen.queryByText(/Waiting for Cardano node/)).toBeNull();
  });
});

describe('formatStopElapsed', () => {
  it('formats milliseconds as minutes and zero-padded seconds', () => {
    expect(formatStopElapsed(0)).toBe('0:00');
    expect(formatStopElapsed(9_999)).toBe('0:09');
    expect(formatStopElapsed(83_000)).toBe('1:23');
    expect(formatStopElapsed(300_000)).toBe('5:00');
  });

  it('treats negative input as zero', () => {
    expect(formatStopElapsed(-5)).toBe('0:00');
  });
});
