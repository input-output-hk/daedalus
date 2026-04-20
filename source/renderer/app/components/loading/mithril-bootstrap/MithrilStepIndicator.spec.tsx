import React from 'react';
import { IntlProvider } from 'react-intl';
import { act, cleanup, render, screen, within } from '@testing-library/react';
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
  bytesDownloaded: number;
  progressItems: MithrilProgressItem[];
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
  afterEach(() => {
    jest.useRealTimers();
  });

  it('uses an open dot for active top-level steps while subitems keep their spinner', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'pending' },
      { id: 'step-2', label: 'Step 2', state: 'pending' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
    ];

    renderComponent('downloading', {
      progressItems,
      bytesDownloaded: 1200 * 1024 * 1024,
      snapshotSize: 1900 * 1024 * 1024,
    });

    const downloadingStep = screen
      .getByText(/downloading$/i)
      .closest('[role="listitem"]');
    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });

    expect(downloadingStep).not.toBeNull();
    expect(downloadingStep?.querySelector('.activeCircle')).not.toBeNull();
    expect(downloadingStep?.querySelector('.iconSpinner')).toBeNull();
    expect(downloadingDetails.querySelector('svg')).not.toBeNull();
  });

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
      snapshotSize: 1900 * 1024 * 1024,
    });

    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });
    const downloadingSnapshot = within(downloadingDetails).getByText(
      /downloading snapshot data/i
    );
    const combinedProgressLabel = within(downloadingDetails).getByText(
      /snapshot files and fast sync/i
    );
    const verifyingDigests = within(downloadingDetails).getByText(
      /verifying snapshot digests/i
    );
    const progressBars = within(downloadingDetails).getAllByRole('progressbar');

    expect(
      downloadingSnapshot.compareDocumentPosition(combinedProgressLabel) &
        Node.DOCUMENT_POSITION_FOLLOWING
    ).toBeTruthy();
    expect(
      combinedProgressLabel.compareDocumentPosition(verifyingDigests) &
        Node.DOCUMENT_POSITION_FOLLOWING
    ).toBeTruthy();
    expect(progressBars).toHaveLength(1);
  });

  it('continues progressing when ancillary downloads overlap with snapshot downloads', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'completed' },
      { id: 'step-2', label: 'Step 2', state: 'completed' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 90,
      ancillaryBytesTotal: 100,
      ancillaryProgress: 90,
      bytesDownloaded: 500,
      progressItems,
      snapshotSize: 1000,
    });

    expect(
      screen.getByRole('progressbar', {
        name: /snapshot files and fast sync: 54%/i,
      })
    ).toBeInTheDocument();
  });

  it('marks the combined progress as complete when fast sync completes before verification starts', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'completed' },
      { id: 'step-2', label: 'Step 2', state: 'completed' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 100,
      ancillaryBytesTotal: 100,
      ancillaryProgress: 99,
      bytesDownloaded: 500,
      progressItems,
      snapshotSize: 1000,
    });

    expect(
      screen.getByRole('progressbar', {
        name: /snapshot files and fast sync: 100%/i,
      })
    ).toBeInTheDocument();
  });

  it('briefly pauses before showing verifying snapshot digests as active', () => {
    jest.useFakeTimers();

    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'completed' },
      { id: 'step-2', label: 'Step 2', state: 'completed' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
      { id: 'step-5', label: 'Step 5', state: 'pending' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 100,
      ancillaryBytesTotal: 100,
      ancillaryProgress: 100,
      bytesDownloaded: 1000,
      progressItems,
      snapshotSize: 1000,
    });

    expect(
      screen.getByRole('progressbar', {
        name: /snapshot files and fast sync: 100%/i,
      })
    ).toBeInTheDocument();

    act(() => {
      jest.advanceTimersByTime(500);
    });

    const verifyingDigests = screen.getByText(/verifying snapshot digests/i);
    const verifyingDigestsItem = verifyingDigests.closest('[role="listitem"]');

    expect(verifyingDigestsItem).toHaveAttribute('aria-current', 'step');
    expect(screen.queryAllByRole('progressbar')).toHaveLength(0);
  });

  it('does not render download progress bars before step-3 becomes active', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'completed' },
      { id: 'step-2', label: 'Step 2', state: 'active' },
      { id: 'step-3', label: 'Step 3', state: 'pending' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 24 * 1024 * 1024,
      ancillaryBytesTotal: 195 * 1024 * 1024,
      ancillaryProgress: 12,
      bytesDownloaded: 1200 * 1024 * 1024,
      progressItems,
      snapshotSize: 1900 * 1024 * 1024,
    });

    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });

    expect(
      within(downloadingDetails).queryByText(/snapshot files and fast sync/i)
    ).not.toBeInTheDocument();
    expect(screen.queryAllByRole('progressbar')).toHaveLength(0);
  });

  it('renders verifying status without progress bars', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'completed' },
      { id: 'step-2', label: 'Step 2', state: 'completed' },
      { id: 'step-3', label: 'Step 3', state: 'completed' },
      { id: 'step-4', label: 'Step 4', state: 'active' },
    ];

    renderComponent('verifying', {
      progressItems,
    });

    const preparingStep = screen
      .getByText(/preparing$/i)
      .closest('[role="listitem"]');
    const downloadingStep = screen
      .getByText(/downloading$/i)
      .closest('[role="listitem"]');

    expect(preparingStep).toHaveClass('stepCompleted');
    expect(preparingStep).not.toHaveClass('stepPending');
    expect(downloadingStep).toHaveClass('stepActive');
    expect(downloadingStep).not.toHaveClass('stepPending');
    expect(screen.queryAllByRole('progressbar')).toHaveLength(0);
    expect(screen.getByText(/verifying snapshot digests/i)).toBeInTheDocument();
  });
});
