import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import MithrilProgressView from './MithrilProgressView';
import { MITHRIL_PROGRESS_HEADING_ID } from './accessibilityIds';

describe('MithrilProgressView', () => {
  const renderComponent = ({
    status = 'unpacking' as const,
    bytesDownloaded,
    snapshotSize,
    bootstrapStartedAt,
  }: {
    status?:
      | 'preparing'
      | 'downloading'
      | 'unpacking'
      | 'converting'
      | 'finalizing'
      | 'completed'
      | 'starting-node';
    bytesDownloaded?: number;
    snapshotSize?: number;
    bootstrapStartedAt?: number | null;
  } = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilProgressView
          status={status}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          bootstrapStartedAt={bootstrapStartedAt}
          onCancel={jest.fn()}
        />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('renders the header and timer', () => {
    renderComponent({
      status: 'downloading',
      bootstrapStartedAt: Date.now() - 65_000,
    });

    const heading = screen.getByRole('heading', {
      name: /fast sync with mithril/i,
    });

    expect(heading).toBeInTheDocument();
    expect(heading).toHaveAttribute('id', MITHRIL_PROGRESS_HEADING_ID);
    expect(
      screen.getByText(
        /snapshot download and verification time can vary based on your network connection and storage performance/i
      )
    ).toBeInTheDocument();
    expect(screen.getByText('1:05')).toBeInTheDocument();
  });

  it('shows default timer when no elapsed seconds provided', () => {
    renderComponent({ status: 'preparing' });

    expect(screen.getByText('0:00')).toBeInTheDocument();
  });

  it('shows the node startup handoff while cardano-node is starting', () => {
    renderComponent({ status: 'starting-node' });

    const statusRegion = screen.getByRole('status');
    const completionHeading = screen.getByRole('heading', {
      name: /starting cardano node/i,
    });

    expect(completionHeading).toBeInTheDocument();
    expect(statusRegion).toHaveAttribute('aria-live', 'polite');
    expect(statusRegion).toHaveAttribute('aria-atomic', 'true');
    expect(
      screen.getByText(/mithril snapshot has been restored/i)
    ).toBeInTheDocument();
  });

  it('does not show completion block for restore-complete state alone', () => {
    renderComponent({ status: 'completed' });

    expect(
      screen.queryByRole('heading', { name: /starting cardano node/i })
    ).not.toBeInTheDocument();
  });

  it('does not show completion block when still downloading', () => {
    renderComponent({ status: 'downloading' });

    expect(
      screen.queryByRole('heading', { name: /starting cardano node/i })
    ).not.toBeInTheDocument();
  });

  it('disables the cancel button while cardano-node is starting', () => {
    renderComponent({ status: 'starting-node' });

    expect(screen.getByRole('button', { name: /cancel/i })).toBeDisabled();
  });

  it('keeps the cancel button enabled in completed restore state', () => {
    renderComponent({ status: 'completed' });

    expect(screen.getByRole('button', { name: /cancel/i })).toBeEnabled();
  });
});
