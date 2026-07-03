import React from 'react';
import { IntlProvider } from 'react-intl';
import { act, cleanup, render, screen, within } from '@testing-library/react';
import '@testing-library/jest-dom';
import type {
  MithrilBootstrapStatus,
  MithrilProgressItem,
} from '../../../../../common/types/mithril-bootstrap.types';
import type { MithrilPartialSyncStatus } from '../../../../../common/types/mithril-partial-sync.types';
import translations from '../../../i18n/locales/en-US.json';
import MithrilStepIndicator from './MithrilStepIndicator';

type TestProps = Partial<{
  ancillaryBytesDownloaded: number;
  ancillaryBytesTotal: number;
  ancillaryProgress: number;
  filesDownloaded: number;
  filesTotal: number;
  progressItems: MithrilProgressItem[];
  snapshotSizeBytes: number;
}>;

describe('MithrilStepIndicator', () => {
  const renderComponent = (
    status: MithrilBootstrapStatus | MithrilPartialSyncStatus = 'finalizing',
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

  it('renders a rotating spinner for the active top-level step while subitems keep their spinner', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'step-1', label: 'Step 1', state: 'pending' },
      { id: 'step-2', label: 'Step 2', state: 'pending' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
      { id: 'step-4', label: 'Step 4', state: 'pending' },
    ];

    renderComponent('downloading', {
      progressItems,
      filesDownloaded: 1200 * 1024 * 1024,
      filesTotal: 1900 * 1024 * 1024,
      snapshotSizeBytes: 1900 * 1024 * 1024,
    });

    const downloadingStep = screen
      .getByText(/downloading$/i)
      .closest('[role="listitem"]');
    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });

    expect(downloadingStep).not.toBeNull();
    // The active top-level step now renders the rotating spinner svg
    // (styles.iconSpinner) instead of the static .activeCircle dot. Under the
    // svg-jest transform react-svg-inline renders a bare <svg> without its
    // wrapper className, so the spinner is asserted via the top-level
    // .iconContainer svg rather than the .iconSpinner class token.
    expect(downloadingStep?.querySelector('.activeCircle')).toBeNull();
    expect(downloadingStep?.querySelector('.iconContainer svg')).not.toBeNull();
    expect(downloadingDetails.querySelector('svg')).not.toBeNull();
  });

  it('renders the preparing step as active during stopping-node (no greyed placeholders)', () => {
    renderComponent('stopping-node', { progressItems: [] });

    const preparingStep = screen
      .getByText(/preparing$/i)
      .closest('[role="listitem"]');

    expect(preparingStep).toHaveClass('stepActive');
    expect(preparingStep).not.toHaveClass('stepPending');
    // Active step shows the rotating spinner svg, not a greyed pending dot.
    expect(preparingStep?.querySelector('.iconContainer svg')).not.toBeNull();
    expect(preparingStep?.querySelector('.pendingCircle')).toBeNull();
    expect(preparingStep?.querySelector('.activeCircle')).toBeNull();
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
      filesDownloaded: 1200 * 1024 * 1024,
      progressItems,
      filesTotal: 1900 * 1024 * 1024,
      snapshotSizeBytes: 1900 * 1024 * 1024,
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
      filesDownloaded: 500,
      progressItems,
      filesTotal: 1000,
      snapshotSizeBytes: 1000,
    });

    expect(
      screen.getByRole('progressbar', {
        name: /snapshot files and fast sync: 54%/i,
      })
    ).toBeInTheDocument();
  });

  it('formats snapshot progress as file counts and fast sync progress as byte sizes', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'Preparing', state: 'completed' },
      { id: 'downloading', label: 'Downloading', state: 'active' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 24 * 1024,
      ancillaryBytesTotal: 195 * 1024,
      filesDownloaded: 901,
      progressItems,
      filesTotal: 25400,
    });

    expect(
      screen.getByText(
        /snapshot files: 901 \/ 25,400 files \| fast sync: 24\.0 kb \/ 195\.0 kb/i
      )
    ).toBeInTheDocument();
  });

  it('shows file counts and a static total-size context when a real snapshot size is available', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'Preparing', state: 'completed' },
      { id: 'downloading', label: 'Downloading', state: 'active' },
      { id: 'step-3', label: 'Step 3', state: 'active' },
    ];

    renderComponent('downloading', {
      filesDownloaded: 142,
      filesTotal: 980,
      snapshotSizeBytes: 9_700_000_000,
      progressItems,
    });

    expect(screen.getByText(/142 \/ 980 files/i)).toBeInTheDocument();
    expect(screen.getByText(/≈ .* total/i)).toBeInTheDocument();
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
      filesDownloaded: 500,
      progressItems,
      filesTotal: 1000,
      snapshotSizeBytes: 1000,
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
      filesDownloaded: 1000,
      progressItems,
      filesTotal: 1000,
      snapshotSizeBytes: 1000,
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
      filesDownloaded: 1200 * 1024 * 1024,
      progressItems,
      filesTotal: 1900 * 1024 * 1024,
      snapshotSizeBytes: 1900 * 1024 * 1024,
    });

    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });

    expect(
      within(downloadingDetails).queryByText(/snapshot files and fast sync/i)
    ).not.toBeInTheDocument();
    expect(screen.queryAllByRole('progressbar')).toHaveLength(0);
  });

  it('renders download progress bars for partial sync top-level downloading status without bootstrap substeps', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'Preparing', state: 'completed' },
      { id: 'downloading', label: 'Downloading', state: 'active' },
    ];

    renderComponent('downloading', {
      ancillaryBytesDownloaded: 24 * 1024 * 1024,
      ancillaryBytesTotal: 195 * 1024 * 1024,
      filesDownloaded: 1200 * 1024 * 1024,
      progressItems,
      filesTotal: 1900 * 1024 * 1024,
      snapshotSizeBytes: 1900 * 1024 * 1024,
    });

    const downloadingDetails = screen.getByRole('list', {
      name: /downloading details/i,
    });

    expect(
      within(downloadingDetails).getByText(/snapshot files and fast sync/i)
    ).toBeInTheDocument();
    expect(screen.getAllByRole('progressbar')).toHaveLength(1);
    expect(
      within(downloadingDetails)
        .getByRole('progressbar')
        .closest('[class*=inlineBar]')
    ).toHaveClass('inlineBarEmphasized');
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

  it('keeps the install substep active while top-level status remains finalizing', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'Preparing', state: 'completed' },
      { id: 'downloading', label: 'Downloading', state: 'completed' },
      { id: 'conversion', label: 'conversion', state: 'completed' },
      { id: 'install-snapshot', label: 'install-snapshot', state: 'completed' },
    ];

    renderComponent('finalizing', {
      progressItems,
    });

    const movingSnapshot = screen.getByText(/moving snapshot to storage/i);
    const movingSnapshotItem = movingSnapshot.closest('[role="listitem"]');

    expect(movingSnapshotItem).toHaveAttribute('aria-current', 'step');
    expect(movingSnapshotItem).toHaveClass('subItemActive');
    expect(movingSnapshotItem).not.toHaveClass('subItemCompleted');
  });

  it('renders a localized label for the verifying stage item instead of its raw id', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'preparing', state: 'completed' },
      { id: 'downloading', label: 'downloading', state: 'completed' },
      { id: 'verifying', label: 'verifying', state: 'active' },
    ];

    renderComponent('verifying', { progressItems });

    expect(screen.getByText('Verifying snapshot...')).toBeInTheDocument();
    expect(screen.queryByText(/^verifying$/)).not.toBeInTheDocument();
  });

  it('renders localized labels for the converting and installing stage items', () => {
    const progressItems: MithrilProgressItem[] = [
      { id: 'preparing', label: 'preparing', state: 'completed' },
      { id: 'downloading', label: 'downloading', state: 'completed' },
      { id: 'verifying', label: 'verifying', state: 'completed' },
      { id: 'converting', label: 'converting', state: 'completed' },
      { id: 'installing', label: 'installing', state: 'active' },
    ];

    renderComponent('installing', { progressItems });

    expect(
      screen.getByText('Converting snapshot format...')
    ).toBeInTheDocument();
    expect(screen.getByText('Installing snapshot...')).toBeInTheDocument();
    expect(screen.queryByText(/^converting$/)).not.toBeInTheDocument();
    expect(screen.queryByText(/^installing$/)).not.toBeInTheDocument();
  });
});
