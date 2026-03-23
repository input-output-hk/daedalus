import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import translations from '../../../i18n/locales/en-US.json';
import MithrilBootstrap from './MithrilBootstrap';

jest.mock('../../chain-storage/ChainStorageLocationPicker', () => {
  return function ChainStorageLocationPickerMock() {
    return <h1>Choose blockchain data location</h1>;
  };
});

jest.mock('./MithrilDecisionView', () => {
  return function MithrilDecisionViewMock(props) {
    return (
      <div>
        <button type="button" onClick={props.onReturnToStorageLocation}>
          Change location
        </button>
        <span>Decision view</span>
      </div>
    );
  };
});

jest.mock('./MithrilProgressView', () => {
  return function MithrilProgressViewMock() {
    return <div>Progress view</div>;
  };
});

jest.mock('./MithrilErrorView', () => {
  return function MithrilErrorViewMock() {
    return <div>Error view</div>;
  };
});

describe('MithrilBootstrap', () => {
  const snapshots: Array<MithrilSnapshotItem> = [
    {
      digest: '12345678abcdef9012345678abcdef90',
      size: 2048,
      createdAt: '2026-03-10T12:00:00.000Z',
      network: 'mainnet',
      cardanoNodeVersion: '10.0.0',
    },
  ];

  const renderComponent = (overrides = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilBootstrap
          status="decision"
          customChainPath="/mnt/current-chain"
          defaultChainPath="/tmp/state/chain"
          defaultChainStorageValidation={{
            isValid: true,
            path: null,
            resolvedPath: '/tmp/state/chain',
          }}
          chainStorageValidation={{
            isValid: true,
            path: '/mnt/current-chain',
            resolvedPath: '/mnt/current-chain',
          }}
          storageLocationConfirmed
          snapshots={snapshots}
          selectedDigest="latest"
          selectedSnapshot={snapshots[0]}
          error={null}
          isFetchingSnapshots={false}
          onSelectSnapshot={jest.fn()}
          onAccept={jest.fn()}
          onDecline={jest.fn()}
          onWipeRetry={jest.fn()}
          onCancel={jest.fn()}
          onConfirmStorageLocation={jest.fn()}
          onReturnToStorageLocation={jest.fn()}
          {...overrides}
        />
      </IntlProvider>
    );

  beforeEach(() => {
    jest.clearAllMocks();
  });

  afterEach(cleanup);

  it('returns to the storage picker from the decision view', () => {
    const onReturnToStorageLocation = jest.fn();

    renderComponent({ onReturnToStorageLocation });

    fireEvent.click(
      screen.getByRole('button', {
        name: /change location/i,
      })
    );

    expect(onReturnToStorageLocation).toHaveBeenCalledTimes(1);
  });

  it('shows the storage picker when storage location is not confirmed', () => {
    renderComponent({ storageLocationConfirmed: false });

    expect(
      screen.getByRole('heading', {
        name: /choose blockchain data location/i,
      })
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /use mithril fast sync/i })
    ).not.toBeInTheDocument();
  });

  it('shows the progress view for active bootstrap statuses', () => {
    renderComponent({ status: 'downloading' });

    expect(screen.getByText('Progress view')).toBeInTheDocument();
    expect(screen.queryByText('Decision view')).not.toBeInTheDocument();
  });

  it('keeps the progress view mounted through bootstrap completion handoff', () => {
    renderComponent({ status: 'completed' });

    expect(screen.getByText('Progress view')).toBeInTheDocument();
    expect(screen.queryByText('Decision view')).not.toBeInTheDocument();
  });

  it('keeps the progress view mounted while cardano-node is starting', () => {
    renderComponent({ status: 'starting-node' });

    expect(screen.getByText('Progress view')).toBeInTheDocument();
    expect(screen.queryByText('Decision view')).not.toBeInTheDocument();
  });

  it('shows the error view when bootstrap fails', () => {
    renderComponent({ status: 'failed' });

    expect(screen.getByText('Error view')).toBeInTheDocument();
    expect(screen.queryByText('Progress view')).not.toBeInTheDocument();
  });

  it('renders progress view when status is verifying', () => {
    renderComponent({ status: 'verifying' });

    expect(screen.getByText('Progress view')).toBeInTheDocument();
    expect(screen.queryByText('Decision view')).not.toBeInTheDocument();
    expect(screen.queryByText('Error view')).not.toBeInTheDocument();
  });
});
