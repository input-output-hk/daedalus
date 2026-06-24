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

const defaultProps = {
  formattedSyncPercentage: '62.50',
  isActionBlocked: false,
  isMithrilPartialSyncWorking: false,
  isSynced: false,
  shouldShowRecommendation: true,
  behindByImmutables: undefined,
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

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril partial sync begins',
      })
    ).toBeInTheDocument();
    expect(onStartMithrilPartialSync).not.toHaveBeenCalled();
  });

  it('returns to the recommendation when confirmation is cancelled', () => {
    renderComponent();

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    screen.getByRole('button', { name: 'Back to diagnostics' }).click();

    expect(
      screen.getByText(
        'Cardano node is currently 62.50% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.'
      )
    ).toBeInTheDocument();
  });

  it('starts partial sync only after confirmation', async () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({ onStartMithrilPartialSync });

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    screen.getByRole('button', { name: 'Start Mithril partial sync' }).click();

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

      screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
      screen
        .getByRole('button', { name: 'Start Mithril partial sync' })
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

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    screen.getByRole('button', { name: 'Start Mithril partial sync' }).click();

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

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril partial sync begins',
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
      screen.getByText(
        'Cardano node is currently 62.50% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.'
      )
    ).toBeInTheDocument();
  });

  it('renders nothing and leaves no header row when the recommendation is gated off', () => {
    const { container } = renderComponent({ shouldShowRecommendation: false });

    expect(
      screen.queryByRole('button', { name: 'Mithril Partial Sync' })
    ).toBeNull();
    expect(container.textContent).not.toMatch(/Mithril Partial Sync/);
  });

  it('opens the confirmation modal on mount when deep-linked, without starting', () => {
    const onStartMithrilPartialSync = jest.fn();
    renderComponent({
      showConfirmationOnOpen: true,
      onStartMithrilPartialSync,
    });

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril partial sync begins',
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
        name: 'Before Mithril partial sync begins',
      })
    ).toBeInTheDocument();
  });

  it('does not deep-link open the confirmation when the action is blocked', () => {
    renderComponent({ showConfirmationOnOpen: true, isActionBlocked: true });

    expect(
      screen.queryByRole('heading', {
        name: 'Before Mithril partial sync begins',
      })
    ).toBeNull();
  });

  it('threads the behind-ness figure into the confirmation modal', () => {
    renderComponent({ showConfirmationOnOpen: true, behindByImmutables: 42 });

    expect(
      screen.getByText(
        'Your node is about 42 immutable files behind the latest verified snapshot.'
      )
    ).toBeInTheDocument();
  });

  it('returns to the recommendation when ESC is pressed on the confirmation', () => {
    renderComponent();

    screen.getByRole('button', { name: 'Mithril Partial Sync' }).click();
    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril partial sync begins',
      })
    ).toBeInTheDocument();

    fireEvent.keyDown(
      document.querySelector('.ReactModal__Content') as Element,
      { key: 'Escape', keyCode: 27 }
    );

    expect(
      screen.getByText(
        'Cardano node is currently 62.50% synced. If catch-up is taking longer than you want, Mithril partial sync can restore verified chain data to help it catch up faster.'
      )
    ).toBeInTheDocument();
  });
});
