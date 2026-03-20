import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen, within } from '@testing-library/react';
import '@testing-library/jest-dom';
import type {
  MithrilBootstrapStatus,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import translations from '../../../i18n/locales/en-US.json';
import MithrilStepIndicator from './MithrilStepIndicator';

type TestProps = Partial<{
  ancillaryBytesDownloaded: number;
  ancillaryBytesTotal: number;
  ancillaryProgress: number;
  ancillaryRemainingSeconds: number;
  bytesDownloaded: number;
  progressItems: MithrilProgressItem[];
  remainingSeconds: number;
  snapshotSize: number;
}>;

describe('MithrilStepIndicator', () => {
  const renderComponent = (
    status: MithrilBootstrapStatus = 'finalizing',
    props: TestProps = {}
  ) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilStepIndicator status={status} {...props} />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('renders the visible Mithril flow as preparing, downloading, and finalizing', () => {
    renderComponent();

    expect(screen.getByText(/preparing/i)).toBeInTheDocument();
    expect(screen.getByText(/downloading/i)).toBeInTheDocument();
    expect(screen.getByText(/finalizing/i)).toBeInTheDocument();
    expect(screen.queryByText(/installing/i)).not.toBeInTheDocument();
  });

  it('maps unpacking status into the visible finalizing step', () => {
    renderComponent('unpacking');

    expect(screen.queryByText(/92.5%/i)).not.toBeInTheDocument();
    expect(screen.queryByText(/installing/i)).not.toBeInTheDocument();
    expect(screen.getByText(/finalizing/i)).toBeInTheDocument();
  });

  it('renders download progress bars between snapshot download and verification steps', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'completed' },
      { id: 'step-2', label: 'Step 2', state: 'completed' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
      { id: 'step-5', label: 'Step 5', state: 'pending' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 24 * 1024 * 1024,
      ancillaryBytesTotal: 195 * 1024 * 1024,
      ancillaryProgress: 12,
      bytesDownloaded: 1200 * 1024 * 1024,
      progressItems,
      remainingSeconds: 872,
      snapshotSize: 1900 * 1024 * 1024,
    });

    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });
    const downloadingSnapshot = within(downloadingDetails).getByText(
      /downloading snapshot data/i
    );
    const snapshotFiles = within(downloadingDetails).getByText(
      /snapshot files/i
    );
    const fastStateSync = within(downloadingDetails).getByText(
      /fast state sync/i
    );
    const verifyingDigests = within(downloadingDetails).getByText(
      /verifying snapshot digests/i
    );

    expect(
      downloadingSnapshot.compareDocumentPosition(snapshotFiles) &
        Node.DOCUMENT_POSITION_FOLLOWING
    ).toBeTruthy();
    expect(
      snapshotFiles.compareDocumentPosition(verifyingDigests) &
        Node.DOCUMENT_POSITION_FOLLOWING
    ).toBeTruthy();
    expect(
      fastStateSync.compareDocumentPosition(verifyingDigests) &
        Node.DOCUMENT_POSITION_FOLLOWING
    ).toBeTruthy();
  });
});
