import React from 'react';
import { IntlProvider } from 'react-intl';
import {
  act,
  cleanup,
  fireEvent,
  render,
  screen,
} from '@testing-library/react';
import '@testing-library/jest-dom';
import translations from '../../../i18n/locales/en-US.json';
import jaTranslations from '../../../i18n/locales/ja-JP.json';
import { isMithrilPartialSyncBlockingNodeStart } from '../../../../../common/types/mithril-partial-sync.types';
import MithrilPartialSyncOverlay, {
  isMithrilPartialSyncOverlayStatus,
} from './MithrilPartialSyncOverlay';

// react-polymorph's Link needs a skin/theme context not provided in this spec; the
// collapsible technical-details section (rendered only when an error message exists)
// pulls it in. Stub it to a plain anchor so error frames with a message can render.
jest.mock('react-polymorph/lib/components/Link', () => ({
  Link: ({ label, onClick }: { label?: string; onClick?: () => void }) => (
    // eslint-disable-next-line jsx-a11y/anchor-is-valid
    <a onClick={onClick}>{label}</a>
  ),
}));

// react-polymorph's PopOver needs a skin/theme context not provided here; the
// disabled-Cancel tooltip (stopping-node) pulls it in. Stub it so the wrapped
// trigger and the tooltip content both render in the test.
jest.mock('react-polymorph/lib/components/PopOver', () => ({
  PopOver: ({ children, content }: { children?: any; content?: any }) => (
    <>
      {children}
      <span>{content}</span>
    </>
  ),
}));

describe('MithrilPartialSyncOverlay', () => {
  const renderComponent = (overrides = {}) =>
    render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilPartialSyncOverlay
          status="downloading"
          progressItems={[]}
          startedAt={Date.now() - 65_000}
          transferProgress={{
            filesDownloaded: 3,
            filesTotal: 9,
            ancillaryBytesDownloaded: 1,
            ancillaryBytesTotal: 2,
          }}
          error={null}
          canRetry={false}
          canRestartNormally={false}
          canWipeAndFullSync={false}
          onCancel={jest.fn()}
          onRetry={jest.fn()}
          onRestartNormally={jest.fn()}
          onWipeAndFullSync={jest.fn()}
          onDismissCompleted={jest.fn()}
          onQuit={jest.fn()}
          onOpenExternalLink={jest.fn()}
          {...overrides}
        />
      </IntlProvider>
    );

  afterEach(cleanup);

  it('renders the progress overlay with partial sync copy', () => {
    renderComponent();

    expect(
      screen.getByRole('heading', { name: /mithril sync/i })
    ).toBeInTheDocument();
    expect(screen.getByText('1:05')).toBeInTheDocument();
  });

  it('renders recovery actions only from allowed backend actions', () => {
    const onRetry = jest.fn();
    const onRestartNormally = jest.fn();

    renderComponent({
      status: 'failed',
      canRetry: true,
      canRestartNormally: true,
      canWipeAndFullSync: false,
      error: null,
      onRetry,
      onRestartNormally,
    });

    fireEvent.click(
      screen.getByRole('button', { name: /retry mithril sync/i })
    );
    fireEvent.click(screen.getByRole('button', { name: /restart node sync/i }));

    expect(onRetry).toHaveBeenCalledTimes(1);
    expect(onRestartNormally).toHaveBeenCalledTimes(1);
    expect(
      screen.queryByRole('button', {
        name: /wipe chain data and do full mithril sync/i,
      })
    ).not.toBeInTheDocument();
  });

  it('renders the live wipe-and-full-sync recovery action and wires its handler (#8/D-702b-8)', () => {
    // Drive from the post-cutover `failed` state, NOT `cancelled` — locked
    // invariant #6 forbids cancellation after cutover and D-702a-2 removed wipe
    // from the pre-cutover cancelled dialogue. With canRetry/canRestartNormally
    // both false, the wipe action resolves to the primary variant from the
    // boolean combo (not the status string).
    const onWipeAndFullSync = jest.fn();

    renderComponent({
      status: 'failed',
      canRetry: false,
      canRestartNormally: false,
      canWipeAndFullSync: true,
      error: null,
      onWipeAndFullSync,
    });

    const wipeButton = screen.getByRole('button', {
      name: /wipe chain data and do full mithril sync/i,
    });
    expect(wipeButton).toBeInTheDocument();

    fireEvent.click(wipeButton);
    expect(onWipeAndFullSync).toHaveBeenCalledTimes(1);
  });

  it('auto-fires finalize on the completed timeout with no Continue button (ADR D-702a-1)', () => {
    jest.useFakeTimers();
    const onDismissCompleted = jest.fn();
    try {
      renderComponent({ status: 'completed', onDismissCompleted });

      // The explicit "Continue to Daedalus" button is gone; the completed frame
      // is a loading-style hand-off (spinner + "Returning to Daedalus...").
      expect(
        screen.queryByRole('button', { name: /continue to daedalus/i })
      ).not.toBeInTheDocument();
      expect(screen.getByText(/returning to daedalus/i)).toBeInTheDocument();

      // Finalize must not fire until the auto-dismiss linger elapses...
      expect(onDismissCompleted).not.toHaveBeenCalled();

      act(() => {
        jest.advanceTimersByTime(4000);
      });

      // ...then exactly once (reset-to-idle + remove staging + clear marker).
      expect(onDismissCompleted).toHaveBeenCalledTimes(1);
    } finally {
      jest.useRealTimers();
    }
  });

  it('catches a rejecting onDismissCompleted from the auto-dismiss timer (no unhandled rejection)', async () => {
    // #2 (D-702b-4): onDismissCompleted now awaits the async finalize IPC, so the
    // timeout wraps it in Promise.resolve(...).catch(...). A finalize rejection
    // must therefore never surface as an unhandled promise rejection.
    jest.useFakeTimers();
    const onDismissCompleted = jest
      .fn()
      .mockRejectedValue(new Error('finalize failed'));
    const onUnhandledRejection = jest.fn();
    process.on('unhandledRejection', onUnhandledRejection);
    try {
      renderComponent({ status: 'completed', onDismissCompleted });

      expect(() => {
        act(() => {
          jest.advanceTimersByTime(4000);
        });
      }).not.toThrow();
      expect(onDismissCompleted).toHaveBeenCalledTimes(1);

      // Give any (improperly) unhandled rejection a real macrotask to surface.
      jest.useRealTimers();
      await new Promise((resolve) => {
        setTimeout(resolve, 0);
      });

      expect(onUnhandledRejection).not.toHaveBeenCalled();
    } finally {
      jest.useRealTimers();
      process.off('unhandledRejection', onUnhandledRejection);
    }
  });

  it('hides Cancel for every post-cutover phase but keeps it pre-cutover', () => {
    (['installing', 'finalizing', 'starting-node'] as const).forEach(
      (status) => {
        const { unmount } = renderComponent({ status });
        expect(
          screen.queryByRole('button', { name: /cancel/i })
        ).not.toBeInTheDocument();
        unmount();
      }
    );

    (['downloading', 'preparing'] as const).forEach((status) => {
      const { unmount } = renderComponent({ status });
      expect(
        screen.getByRole('button', { name: /cancel/i })
      ).toBeInTheDocument();
      unmount();
    });
  });

  it('renders cancelling as a cleanup progress frame with no timer, waterfall, or actions', () => {
    renderComponent({ status: 'cancelling' });

    expect(
      screen.getByRole('heading', { level: 2, name: 'Cleaning up...' })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/cleaning up mithril sync before you continue/i)
    ).toBeInTheDocument();
    expect(screen.queryByText(/elapsed time:/i)).not.toBeInTheDocument();
    expect(
      screen.queryByLabelText(/mithril sync progress/i)
    ).not.toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /cancel/i })
    ).not.toBeInTheDocument();
  });

  it('renders a defensive Quit fallback only when no recovery actions are available', () => {
    const onQuit = jest.fn();
    const { unmount } = renderComponent({
      status: 'failed',
      error: null,
      onQuit,
    });

    const quitButton = screen.getByRole('button', { name: /quit daedalus/i });
    fireEvent.click(quitButton);
    expect(onQuit).toHaveBeenCalledTimes(1);
    unmount();

    renderComponent({ status: 'failed', error: null, canRetry: true });
    expect(
      screen.queryByRole('button', { name: /quit daedalus/i })
    ).not.toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /retry mithril sync/i })
    ).toBeInTheDocument();
  });

  it('disables Cancel with an explanatory tooltip during stopping-node', () => {
    renderComponent({ status: 'stopping-node' });

    expect(screen.getByRole('button', { name: /cancel/i })).toBeDisabled();
    expect(
      screen.getByText(/cancellation available once the node has stopped/i)
    ).toBeInTheDocument();
  });

  it('does not reuse the bootstrap byte-progress bar for partial sync file counts', () => {
    renderComponent({
      progressItems: [
        {
          id: 'step-3',
          label: 'Downloading snapshot data',
          state: 'active',
        },
      ],
      transferProgress: {
        filesDownloaded: 3,
        filesTotal: 9,
      },
    });

    expect(
      screen.getByText(/snapshot files: .*fast sync:/i)
    ).toBeInTheDocument();
  });

  it('shows the trimmed cancelled recovery actions with the primary on the right and no wipe', () => {
    const onRetry = jest.fn();
    const onRestartNormally = jest.fn();

    renderComponent({
      status: 'cancelled',
      canRetry: true,
      canRestartNormally: true,
      // D-702a-2: the cancelled (pre-cutover) dialogue no longer offers wipe.
      canWipeAndFullSync: false,
      onRetry,
      onRestartNormally,
    });

    fireEvent.click(
      screen.getByRole('button', { name: /retry mithril sync \(fast\)/i })
    );
    fireEvent.click(
      screen.getByRole('button', { name: /restart node sync \(slow\)/i })
    );

    expect(onRetry).toHaveBeenCalledTimes(1);
    expect(onRestartNormally).toHaveBeenCalledTimes(1);
    expect(
      screen.queryByRole('button', {
        name: /wipe chain data and do full mithril sync/i,
      })
    ).not.toBeInTheDocument();

    // the primary action ("Retry Mithril Sync (fast)") renders last/rightmost; the
    // secondary ("Restart Node Sync (slow)") renders first/left.
    const actionLabels = screen
      .getAllByRole('button')
      .map((button) => button.textContent);
    expect(actionLabels).toEqual([
      'Restart Node Sync (slow)',
      'Retry Mithril Sync (fast)',
    ]);
  });

  it('shows bespoke copy for a mapped error code and never the raw backend message as the title', () => {
    renderComponent({
      status: 'failed',
      error: {
        message: '{"raw":"mithril-client json"}',
        code: 'PARTIAL_SYNC_LATEST_DRIFT',
        stage: 'preparing',
      },
    });
    // the primary (level-1) error heading is the bespoke localized title
    expect(
      screen.getByRole('heading', {
        level: 1,
        name: /verified mithril snapshot moved on/i,
      })
    ).toBeInTheDocument();
    expect(
      screen.getByText(/retry mithril sync to use the refreshed snapshot/i)
    ).toBeInTheDocument();
    // the raw backend message is never promoted to the primary error title
    expect(
      screen.queryByRole('heading', { level: 1, name: /mithril-client json/i })
    ).not.toBeInTheDocument();
  });

  it('gives cancelled a calmer hint distinct from failed, rendered as body text', () => {
    const { unmount } = renderComponent({ status: 'cancelled', error: null });
    const cancelledHint = screen.getByText(/was stopped before it finished/i);
    expect(cancelledHint).toBeInTheDocument();
    // the cancelled hint is promoted to normal body text (a <p>), not the subtext <div>
    expect(cancelledHint.tagName).toBe('P');
    unmount();
    renderComponent({ status: 'failed', error: null });
    expect(
      screen.queryByText(/was stopped before it finished/i)
    ).not.toBeInTheDocument();
  });
});

describe('isMithrilPartialSyncOverlayStatus', () => {
  it('includes the stopping-node handoff and later overlay states', () => {
    expect(isMithrilPartialSyncOverlayStatus('stopping-node')).toBe(true);
    expect(isMithrilPartialSyncOverlayStatus('cancelling')).toBe(true);
    expect(isMithrilPartialSyncOverlayStatus('preparing')).toBe(true);
    expect(isMithrilPartialSyncOverlayStatus('failed')).toBe(true);
  });
});

describe('isMithrilPartialSyncBlockingNodeStart', () => {
  it('blocks node start while cancellation cleanup is in progress', () => {
    expect(isMithrilPartialSyncBlockingNodeStart('cancelling')).toBe(true);
  });
});

describe('Mithril partial sync locale strings', () => {
  it('ships polished runtime strings without placeholder markers', () => {
    const keys = Object.keys(translations).filter((key) =>
      key.startsWith('loading.mithrilPartialSync.')
    );

    keys.forEach((key) => {
      expect(translations[key]).not.toMatch(/^!!!/);
      expect(jaTranslations[key]).not.toMatch(/^!!!/);
    });
  });
});
