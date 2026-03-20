import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import MithrilProgressView from './MithrilProgressView';

describe('MithrilProgressView', () => {
  const renderComponent = ({
    status = 'unpacking' as const,
    progress = 92.5,
    bytesDownloaded,
    snapshotSize,
    throughputBps,
    remainingSeconds,
    overallElapsedSeconds,
  }: {
    status?:
      | 'preparing'
      | 'downloading'
      | 'unpacking'
      | 'converting'
      | 'finalizing'
      | 'completed';
    progress?: number;
    bytesDownloaded?: number;
    snapshotSize?: number;
    throughputBps?: number;
    remainingSeconds?: number;
    overallElapsedSeconds?: number;
  } = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilProgressView
          status={status}
          progress={progress}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          throughputBps={throughputBps}
          remainingSeconds={remainingSeconds}
          overallElapsedSeconds={overallElapsedSeconds}
          onCancel={jest.fn()}
        />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('renders the header and timer', () => {
    renderComponent({
      status: 'downloading',
      progress: 49.9,
      overallElapsedSeconds: 65,
    });

    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toBeInTheDocument();
    expect(screen.getByText('1:05')).toBeInTheDocument();
  });

  it('shows default timer when no elapsed seconds provided', () => {
    renderComponent({ status: 'preparing', progress: 0 });

    expect(screen.getByText('0:00')).toBeInTheDocument();
  });

  it('shows the node startup handoff after bootstrap completes', () => {
    renderComponent({ status: 'completed', progress: 100 });

    expect(
      screen.getByRole('heading', { name: /starting cardano-node/i })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/mithril snapshot has been restored/i)
    ).toBeInTheDocument();
  });

  it('does not show completion block when not completed', () => {
    renderComponent({ status: 'downloading', progress: 50 });

    expect(
      screen.queryByRole('heading', { name: /starting cardano-node/i })
    ).not.toBeInTheDocument();
  });

  it('disables the cancel button when bootstrap is completed', () => {
    renderComponent({ status: 'completed', progress: 100 });

    expect(screen.getByRole('button', { name: /cancel/i })).toBeDisabled();
  });
});
