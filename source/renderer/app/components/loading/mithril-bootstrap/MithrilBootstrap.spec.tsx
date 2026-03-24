import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import type { MithrilSnapshotItem } from '../../../../../common/types/mithril-bootstrap.types';
import translations from '../../../i18n/locales/en-US.json';
import MithrilBootstrap from './MithrilBootstrap';
import {
  MITHRIL_CHAIN_STORAGE_HEADING_ID,
  MITHRIL_DECISION_HEADING_ID,
  MITHRIL_ERROR_HEADING_ID,
  MITHRIL_PROGRESS_HEADING_ID,
  MITHRIL_SNAPSHOT_DETAILS_HEADING_ID,
  MITHRIL_SNAPSHOT_SELECTOR_HEADING_ID,
} from './accessibilityIds';

jest.mock('./MithrilSnapshotSelector', () => {
  return function MithrilSnapshotSelectorMock() {
    return (
      <div role="group" aria-label="Snapshot selection">
        <h2 id={MITHRIL_SNAPSHOT_SELECTOR_HEADING_ID} tabIndex={-1}>
          Snapshot
        </h2>
      </div>
    );
  };
});

jest.mock('./MithrilSnapshotDetails', () => {
  return function MithrilSnapshotDetailsMock() {
    return (
      <section aria-labelledby={MITHRIL_SNAPSHOT_DETAILS_HEADING_ID}>
        <h2 id={MITHRIL_SNAPSHOT_DETAILS_HEADING_ID} tabIndex={-1}>
          Snapshot details
        </h2>
      </section>
    );
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

    fireEvent.click(screen.getByText(/change location/i));

    expect(onReturnToStorageLocation).toHaveBeenCalledTimes(1);
  });

  it('adds dialog semantics and labels the decision view from the active heading', () => {
    renderComponent();

    const dialog = screen.getByRole('dialog', {
      name: /fast sync with mithril/i,
    });
    const heading = screen.getByRole('heading', {
      name: /fast sync with mithril/i,
    });

    expect(dialog).toHaveAttribute('aria-modal', 'true');
    expect(dialog).toHaveAttribute(
      'aria-labelledby',
      MITHRIL_DECISION_HEADING_ID
    );
    expect(heading).toHaveAttribute('id', MITHRIL_DECISION_HEADING_ID);
    expect(
      screen.getByRole('group', { name: /snapshot selection/i })
    ).toBeInTheDocument();
    expect(
      screen.getByRole('region', { name: /snapshot details/i })
    ).toBeInTheDocument();
  });

  it('shows the storage picker when storage location is not confirmed', () => {
    renderComponent({ storageLocationConfirmed: false });

    const dialog = screen.getByRole('dialog', {
      name: /select blockchain data location/i,
    });

    expect(
      screen.getByRole('heading', {
        name: /select blockchain data location/i,
      })
    ).toBeInTheDocument();
    expect(dialog).toHaveAttribute(
      'aria-labelledby',
      MITHRIL_CHAIN_STORAGE_HEADING_ID
    );
    expect(
      screen.queryByRole('button', { name: /use mithril fast sync/i })
    ).not.toBeInTheDocument();
  });

  it('shows the progress view for active bootstrap statuses', () => {
    renderComponent({ status: 'downloading' });

    const dialog = screen.getByRole('dialog', {
      name: /fast sync with mithril/i,
    });

    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toHaveAttribute('id', MITHRIL_PROGRESS_HEADING_ID);
    expect(dialog).toHaveAttribute(
      'aria-labelledby',
      MITHRIL_PROGRESS_HEADING_ID
    );
  });

  it('keeps the progress view mounted through bootstrap completion handoff', () => {
    renderComponent({ status: 'completed' });

    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toBeInTheDocument();
  });

  it('keeps the progress view mounted while cardano-node is starting', () => {
    renderComponent({ status: 'starting-node' });

    expect(
      screen.getByRole('heading', { name: /starting cardano-node/i })
    ).toBeInTheDocument();
  });

  it('shows the error view when bootstrap fails', () => {
    renderComponent({ status: 'failed' });

    const dialog = screen.getByRole('dialog');
    const alert = screen.getByRole('alert');

    expect(alert).toBeInTheDocument();
    expect(
      screen.getByRole('heading', { name: /mithril bootstrap failed/i })
    ).toHaveAttribute('id', MITHRIL_ERROR_HEADING_ID);
    expect(dialog).toHaveAttribute('aria-labelledby', MITHRIL_ERROR_HEADING_ID);
  });

  it('renders progress view when status is verifying', () => {
    renderComponent({ status: 'verifying' });

    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('heading', { name: /mithril bootstrap failed/i })
    ).not.toBeInTheDocument();
  });
});
