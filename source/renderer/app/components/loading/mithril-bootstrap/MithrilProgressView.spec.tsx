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
    elapsedSeconds,
    remainingSeconds,
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
    elapsedSeconds?: number;
    remainingSeconds?: number;
  } = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilProgressView
          status={status}
          progress={progress}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          throughputBps={throughputBps}
          elapsedSeconds={elapsedSeconds}
          remainingSeconds={remainingSeconds}
          onCancel={jest.fn()}
        />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('restores download metadata during transfer', () => {
    renderComponent({
      status: 'downloading',
      progress: 49.9,
      bytesDownloaded: 1024,
      snapshotSize: 2048,
      throughputBps: 1024,
      elapsedSeconds: 65,
      remainingSeconds: 125,
    });

    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toBeInTheDocument();
    expect(screen.getByText('!!!Downloaded')).toBeInTheDocument();
    expect(screen.getByText(/1\.0 KB \/ 2\.0 KB/i)).toBeInTheDocument();
    expect(screen.getByText('!!!Transfer rate')).toBeInTheDocument();
    expect(screen.getByText(/1\.0 KB\/s/i)).toBeInTheDocument();
    expect(screen.getByText('!!!Elapsed')).toBeInTheDocument();
    expect(screen.getByText('1:05')).toBeInTheDocument();
    expect(screen.getByText('!!!Time remaining')).toBeInTheDocument();
    expect(screen.getByText('2:05')).toBeInTheDocument();
  });

  it('keeps metadata informative during local processing', () => {
    renderComponent({
      status: 'unpacking',
      progress: 92.5,
      snapshotSize: 2048,
      elapsedSeconds: 301,
    });

    expect(
      screen.getByText(
        /moving the restored snapshot into your blockchain data location/i
      )
    ).toBeInTheDocument();
    expect(screen.getByText(/2\.0 KB \/ 2\.0 KB/i)).toBeInTheDocument();
    expect(screen.getByText('!!!Local processing')).toBeInTheDocument();
    expect(screen.getByText('5:01')).toBeInTheDocument();
    expect(
      screen.getByText(/completing the final local restore/i)
    ).toBeInTheDocument();
  });

  it('shows verification copy for the late download verification phase', () => {
    renderComponent({
      status: 'downloading',
      progress: 89.9,
    });

    expect(
      screen.getByRole('heading', { name: /fast sync with mithril/i })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/verifying the downloaded snapshot/i)
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        /checking the downloaded snapshot before daedalus restores it/i
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/pause near 90%/i)).not.toBeInTheDocument();
    expect(screen.getByText('!!!Downloaded')).toBeInTheDocument();
  });

  it('shows the node startup handoff after bootstrap completes', () => {
    renderComponent({ status: 'completed', progress: 100 });

    expect(
      screen.getByRole('heading', { name: /starting daedalus/i })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/the snapshot has been restored/i)
    ).toBeInTheDocument();
  });
});
