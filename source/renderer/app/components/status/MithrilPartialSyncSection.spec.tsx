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

import translations from '../../i18n/locales/en-US.json';
import { logger } from '../../utils/logging';
import MithrilPartialSyncSection from './MithrilPartialSyncSection';

jest.mock('../../utils/logging', () => ({
  logger: {
    warn: jest.fn(),
  },
}));

// Render both the trigger (children) and the tooltip body (content) so the
// hover-only recommendation copy is assertable without simulating hover.
jest.mock('react-polymorph/lib/components/PopOver', () => ({
  PopOver: ({
    children,
    content,
  }: {
    children: React.ReactNode;
    content: React.ReactNode;
  }) => (
    <>
      {children}
      {content}
    </>
  ),
}));

const defaultProps = {
  isActionBlocked: false,
  isMithrilPartialSyncWorking: false,
  shouldShowRecommendation: true,
  behindByEpochs: undefined,
  showConfirmationOnOpen: false,
  onRestoreFocus: jest.fn(),
  onStartMithrilPartialSync: jest.fn(),
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <MithrilPartialSyncSection {...defaultProps} {...overrides} />
    </IntlProvider>
  );

describe('MithrilPartialSyncSection', () => {
  afterEach(cleanup);

  it('opens confirmation first and does not start before confirm', () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();
    expect(onStartMithrilPartialSync).not.toHaveBeenCalled();
  });

  it('returns to the recommendation when confirmation is cancelled', () => {
    renderComponent();

    screen.getByRole('button', { name: 'Mithril Sync' }).click();
    screen.getByRole('button', { name: 'Back to diagnostics' }).click();

    expect(
      screen.getByRole('button', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('heading', { name: 'Before Mithril Sync begins' })
    ).toBeNull();
  });

  it('exposes the recommendation copy as the Mithril Sync button tooltip', () => {
    renderComponent();

    expect(
      screen.getByText(
        'If Cardano Node syncing is taking longer than you want, Mithril Sync can restore verified chain data to help speed up the sync.'
      )
    ).toBeInTheDocument();
    // The always-visible ready-hint was removed.
    expect(
      screen.queryByText(/Review what will happen before Daedalus/)
    ).toBeNull();
  });

  it('disables the button and shows only the blocked hint when blocked', () => {
    renderComponent({ isActionBlocked: true });

    expect(
      screen.getByRole('button', { name: 'Mithril Sync' })
    ).toBeDisabled();
    expect(
      screen.getByText('Unavailable while Mithril work is already active.')
    ).toBeInTheDocument();
  });

  it('starts partial sync only after confirmation', async () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();
    screen.getByRole('button', { name: 'Start Mithril Sync' }).click();

    await waitFor(() => {
      expect(onStartMithrilPartialSync).toHaveBeenCalledTimes(1);
    });
  });

  it('does not update state when start resolves after unmount', async () => {
    let resolveStart = () => {};
    const startPromise = new Promise<void>((resolve) => {
      resolveStart = resolve;
    });
    const onStartMithrilPartialSync = jest.fn(() => startPromise);
    const consoleErrorSpy = jest
      .spyOn(console, 'error')
      .mockImplementation(() => {});
    let didLogUnmountedUpdate = false;

    try {
      const { unmount } = renderComponent({ onStartMithrilPartialSync });

      screen.getByRole('button', { name: 'Mithril Sync' }).click();
      screen
        .getByRole('button', { name: 'Start Mithril Sync' })
        .click();

      await waitFor(() => {
        expect(onStartMithrilPartialSync).toHaveBeenCalledTimes(1);
      });

      unmount();

      await act(async () => {
        resolveStart();
        await startPromise;
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

  it('keeps confirmation open and shows concrete start failure', async () => {
    const onStartMithrilPartialSync = jest
      .fn()
      .mockRejectedValue(
        new Error('Mithril partial sync is disabled by launcher configuration.')
      );

    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();
    screen.getByRole('button', { name: 'Start Mithril Sync' }).click();

    await waitFor(() => {
      expect(logger.warn).toHaveBeenCalledWith(
        'MithrilPartialSyncSection: Mithril partial sync start rejected after confirmation',
        {
          error: expect.any(Error),
        }
      );
    });

    expect(
      screen.getByText(
        'Mithril partial sync is disabled by launcher configuration.'
      )
    ).toBeInTheDocument();
  });

  it('closes confirmation when partial sync becomes active externally', () => {
    const { rerender } = render(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilPartialSyncSection {...defaultProps} />
      </IntlProvider>
    );

    screen.getByRole('button', { name: 'Mithril Sync' }).click();
    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();

    rerender(
      <IntlProvider locale="en-US" messages={translations}>
        <MithrilPartialSyncSection
          {...defaultProps}
          isMithrilPartialSyncWorking
        />
      </IntlProvider>
    );

    expect(
      screen.getByRole('button', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('heading', { name: 'Before Mithril Sync begins' })
    ).toBeNull();
  });

  it('renders nothing and leaves no header row when the recommendation is gated off', () => {
    const { container } = renderComponent({ shouldShowRecommendation: false });

    expect(
      screen.queryByRole('button', { name: 'Mithril Sync' })
    ).toBeNull();
    expect(container.textContent).not.toMatch(/Mithril Sync/);
  });

  it('opens the confirmation modal on mount when deep-linked, without starting', () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({
      showConfirmationOnOpen: true,
      onStartMithrilPartialSync,
    });

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();
    expect(onStartMithrilPartialSync).not.toHaveBeenCalled();
  });

  it('deep-link open works even when the recommendation is gated off', () => {
    renderComponent({
      showConfirmationOnOpen: true,
      shouldShowRecommendation: false,
    });

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();
  });

  it('does not deep-link open the confirmation when the action is blocked', () => {
    renderComponent({ showConfirmationOnOpen: true, isActionBlocked: true });

    expect(
      screen.queryByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeNull();
  });

  it('threads the epochs figure into the confirmation modal without any sync-%', () => {
    renderComponent({ showConfirmationOnOpen: true, behindByEpochs: 3 });

    expect(
      screen.getByText(
        'Your node is about 3 epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/% synced/)).toBeNull();
  });

  it('returns to the recommendation when ESC is pressed on the confirmation', () => {
    renderComponent();

    screen.getByRole('button', { name: 'Mithril Sync' }).click();
    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();

    fireEvent.keyDown(
      document.querySelector('.ReactModal__Content') as Element,
      { key: 'Escape', keyCode: 27 }
    );

    expect(
      screen.getByRole('button', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('heading', { name: 'Before Mithril Sync begins' })
    ).toBeNull();
  });
});
