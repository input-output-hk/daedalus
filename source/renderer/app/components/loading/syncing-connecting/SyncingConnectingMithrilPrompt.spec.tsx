import React from 'react';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
  waitFor,
} from '@testing-library/react';
import '@testing-library/jest-dom';

import translations from '../../../i18n/locales/en-US.json';
import { logger } from '../../../utils/logging';
import SyncingConnectingMithrilPrompt from './SyncingConnectingMithrilPrompt';
import styles from './SyncingConnectingMithrilPrompt.scss';

jest.mock('../../../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
  },
}));

const defaultProps = {
  behindByEpochs: undefined,
  onStart: jest.fn(() => Promise.resolve()),
  onDismiss: jest.fn(),
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <SyncingConnectingMithrilPrompt {...defaultProps} {...overrides} />
    </IntlProvider>
  );

const clickButton = (name: string) =>
  fireEvent.click(screen.getByRole('button', { name }));

describe('SyncingConnectingMithrilPrompt', () => {
  afterEach(cleanup);

  it('renders the epochs body and benefit line for a known figure', () => {
    const { container } = renderComponent({ behindByEpochs: 3 });

    expect(
      screen.getByRole('heading', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(
      screen.getByText('Your node is about 3 epochs behind.')
    ).toBeInTheDocument();
    expect(
      screen.getByText('Mithril can catch you up faster than Blockchain Sync.')
    ).toBeInTheDocument();
    expect(container.textContent).not.toMatch(/immutable/i);
    expect(container.textContent).not.toMatch(/partial sync/i);
  });

  it('uses singular epoch phrasing when exactly one epoch behind', () => {
    renderComponent({ behindByEpochs: 1 });

    expect(
      screen.getByText('Your node is about 1 epoch behind.')
    ).toBeInTheDocument();
  });

  it('renders the unknown-figure fallback when no epochs figure is available', () => {
    const { container } = renderComponent({ behindByEpochs: undefined });

    expect(
      screen.getByText('Your node is behind the blockchain tip.')
    ).toBeInTheDocument();
    expect(container.textContent).not.toMatch(/undefined/);
  });

  it('shows the capitalized "Mithril Sync" handoff note in the choice view', () => {
    renderComponent({ behindByEpochs: 3 });

    expect(screen.getByText('Note:')).toBeInTheDocument();
    expect(
      screen.getByText(
        'If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)'
      )
    ).toBeInTheDocument();
  });

  it('shows the Cmd + D shortcut in the handoff note on macOS', () => {
    const originalIsMacOS = global.environment.isMacOS;
    global.environment.isMacOS = true;
    try {
      renderComponent({ behindByEpochs: 3 });

      expect(
        screen.getByText(
          'If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Cmd + D)'
        )
      ).toBeInTheDocument();
    } finally {
      global.environment.isMacOS = originalIsMacOS;
    }
  });

  it('uses the local Mithril action chrome in the choice view', () => {
    renderComponent({ behindByEpochs: 3 });

    expect(
      screen.getByRole('button', { name: 'Blockchain Sync (slow)' })
    ).toHaveClass(styles.secondaryAction);
    expect(
      screen.getByRole('button', { name: 'Mithril Sync (fast)' })
    ).toHaveClass(styles.primaryAction);
  });

  it('shows the shared canonical process summary in the confirm view', () => {
    const { container } = renderComponent({ behindByEpochs: 3 });

    clickButton('Mithril Sync (fast)');

    expect(
      screen.getByRole('heading', { name: 'Mithril Sync Process' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(
        'For this process to begin your Cardano node will need to be shut down. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks.'
      )
    ).toBeInTheDocument();
    expect(container.textContent).not.toMatch(/so you catch up faster/i);
  });

  it('uses the local Mithril action chrome in the confirm view', () => {
    renderComponent({ behindByEpochs: 3 });

    clickButton('Mithril Sync (fast)');

    expect(screen.getByRole('button', { name: 'Cancel' })).toHaveClass(
      styles.secondaryAction
    );
    expect(screen.getByRole('button', { name: 'Start now' })).toHaveClass(
      styles.primaryAction
    );
  });

  it('does NOT start when the fast button is clicked — only reveals the confirm view', () => {
    const onStart = jest.fn(() => Promise.resolve());
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');

    expect(
      screen.getByRole('button', { name: 'Start now' })
    ).toBeInTheDocument();
    expect(onStart).not.toHaveBeenCalled();
  });

  it('dismisses for the session when the standard button is clicked', () => {
    const onStart = jest.fn(() => Promise.resolve());
    const onDismiss = jest.fn();
    renderComponent({ behindByEpochs: 3, onStart, onDismiss });

    clickButton('Blockchain Sync (slow)');

    expect(onDismiss).toHaveBeenCalledTimes(1);
    expect(onStart).not.toHaveBeenCalled();
  });

  it('starts exactly once from the confirm view "Start now"', async () => {
    const onStart = jest.fn(() => Promise.resolve());
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Start now');

    await waitFor(() => {
      expect(onStart).toHaveBeenCalledTimes(1);
    });
  });

  it('returns to the choice view when "Cancel" is clicked, without starting', () => {
    const onStart = jest.fn(() => Promise.resolve());
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Cancel');

    expect(
      screen.getByRole('button', { name: 'Mithril Sync (fast)' })
    ).toBeInTheDocument();
    expect(onStart).not.toHaveBeenCalled();
  });

  it('shows the mapped copy when the start rejection carries a known error code', async () => {
    const onStart = jest
      .fn()
      .mockRejectedValue(new Error('PARTIAL_SYNC_DISABLED'));
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Start now');

    await waitFor(() => {
      expect(logger.warn).toHaveBeenCalledWith(
        'SyncingConnectingMithrilPrompt: Mithril sync start rejected after confirmation',
        { error: expect.any(Error) }
      );
    });

    expect(screen.getByText('Mithril Sync failed')).toBeInTheDocument();
    expect(screen.queryByText('PARTIAL_SYNC_DISABLED')).not.toBeInTheDocument();
  });

  it('shows the shared fallback when the start rejection carries no known code', async () => {
    const onStart = jest
      .fn()
      .mockRejectedValue(
        new Error('Mithril sync is disabled by launcher configuration.')
      );
    renderComponent({ behindByEpochs: 3, onStart });

    clickButton('Mithril Sync (fast)');
    clickButton('Start now');

    await waitFor(() => {
      expect(
        screen.getByText('Unable to start Mithril Sync.')
      ).toBeInTheDocument();
    });
    expect(
      screen.queryByText('Mithril sync is disabled by launcher configuration.')
    ).not.toBeInTheDocument();
  });

  it('does not update state when start rejects after unmount', async () => {
    let rejectStart: (error: Error) => void = () => {};
    const startPromise = new Promise<void>((_resolve, reject) => {
      rejectStart = reject;
    });
    const onStart = jest.fn(() => startPromise);
    const consoleErrorSpy = jest
      .spyOn(console, 'error')
      .mockImplementation(() => {});
    let didLogUnmountedUpdate = false;

    try {
      const { unmount } = renderComponent({ behindByEpochs: 3, onStart });

      clickButton('Mithril Sync (fast)');
      clickButton('Start now');

      await waitFor(() => {
        expect(onStart).toHaveBeenCalledTimes(1);
      });

      unmount();

      await act(async () => {
        rejectStart(new Error('late failure'));
        await startPromise.catch(() => {});
      });

      didLogUnmountedUpdate = consoleErrorSpy.mock.calls.some(([message]) =>
        String(message).includes(
          "Can't perform a React state update on an unmounted component"
        )
      );
    } finally {
      consoleErrorSpy.mockRestore();
    }

    expect(didLogUnmountedUpdate).toBe(false);
  });
});
