import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, fireEvent, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

import translations from '../../i18n/locales/en-US.json';
import MithrilPartialSyncConfirmation from './MithrilPartialSyncConfirmation';

const defaultProps = {
  isActionBlocked: false,
  startError: null,
  behindByEpochs: undefined,
  onCancel: jest.fn(),
  onConfirm: jest.fn(),
};

const renderComponent = (overrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <MithrilPartialSyncConfirmation {...defaultProps} {...overrides} />
    </IntlProvider>
  );

describe('MithrilPartialSyncConfirmation', () => {
  afterEach(cleanup);

  it('renders as a focus-trapped modal with a scrim', () => {
    renderComponent();

    expect(
      screen.getByRole('heading', {
        name: 'Before Mithril Sync begins',
      })
    ).toBeInTheDocument();
    // A real react-modal renders a scrim overlay rather than the old inline div.
    expect(document.querySelector('.ReactModal__Overlay')).not.toBeNull();
  });

  it('returns to diagnostics on ESC without exposing a full-dialog close', () => {
    const onCancel = jest.fn();
    renderComponent({ onCancel });

    const modalContent = document.querySelector('.ReactModal__Content');
    expect(modalContent).not.toBeNull();
    fireEvent.keyDown(modalContent as Element, {
      key: 'Escape',
      keyCode: 27,
    });

    expect(onCancel).toHaveBeenCalledTimes(1);
  });

  it('renders the epochs-only behind-ness line without any sync-% reference', () => {
    renderComponent({ behindByEpochs: 3 });

    const primary = screen.getByText(
      'Your node is about 3 epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.'
    );

    expect(primary).toBeInTheDocument();
    // Sync-% was removed from the confirmation behind-ness per D-A.
    expect(screen.queryByText(/% synced/)).toBeNull();
    // Never expose the internal "immutable files" unit to the user.
    expect(screen.queryByText(/immutable/i)).toBeNull();
  });

  it('falls back to generic behind-ness copy when the figure is unavailable', () => {
    renderComponent({
      behindByEpochs: undefined,
    });

    expect(
      screen.getByText(
        'Your node is behind the latest verified snapshot. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/% synced/)).toBeNull();
    expect(screen.queryByText(/undefined/)).toBeNull();
  });

  it('renders the canonical shutdown/restore/restart subtext and drops the old step copy', () => {
    renderComponent();

    const body = document.querySelector('.mithrilPartialSyncConfirmationBody');
    const paragraphs = body?.querySelectorAll('p');

    expect(body).not.toBeNull();
    expect(paragraphs).toHaveLength(2);
    expect(paragraphs?.[0]).toHaveClass('mithrilPartialSyncConfirmationBehind');
    expect(paragraphs?.[1]).toHaveClass(
      'mithrilPartialSyncConfirmationRecovery'
    );

    expect(
      screen.getByText(
        'For this process to begin your Cardano node will need to be shutdown. Mithril will then be used to sync the verified chain data. On Mithril Sync completion, the node will be restarted to sync the remaining blocks.'
      )
    ).toBeInTheDocument();
    // The verbose what-happens step copy was removed per D-702a-4.
    expect(screen.queryByText(/Daedalus stops Cardano node/)).toBeNull();
    expect(
      screen.queryByText(/downloads and verifies Mithril data/)
    ).toBeNull();
  });

  it('gives the primary and secondary buttons distinct styles', () => {
    renderComponent();

    const startButton = screen.getByRole('button', {
      name: 'Start Mithril Sync',
    });
    const backButton = screen.getByRole('button', {
      name: 'Back to diagnostics',
    });

    expect(startButton).toHaveClass('primary');
    expect(startButton).toHaveClass(
      'mithrilPartialSyncConfirmationPrimaryButton'
    );
    expect(startButton).not.toHaveClass('flat');

    expect(backButton).toHaveClass('flat');
    expect(backButton).toHaveClass(
      'mithrilPartialSyncConfirmationSecondaryButton'
    );
    expect(backButton).not.toHaveClass('primary');
  });

  it('starts only via the primary confirm button', () => {
    const onConfirm = jest.fn();
    const onCancel = jest.fn();
    renderComponent({ onConfirm, onCancel });

    screen.getByRole('button', { name: 'Start Mithril Sync' }).click();

    expect(onConfirm).toHaveBeenCalledTimes(1);
    expect(onCancel).not.toHaveBeenCalled();
  });

  it('does not start when the action is blocked', () => {
    const onConfirm = jest.fn();
    renderComponent({ onConfirm, isActionBlocked: true });

    const startButton = screen.getByRole('button', {
      name: 'Start Mithril Sync',
    });
    expect(startButton).toBeDisabled();
    startButton.click();

    expect(onConfirm).not.toHaveBeenCalled();
  });

  it('renders the start error when one is supplied', () => {
    renderComponent({
      startError: 'Mithril partial sync is disabled by launcher configuration.',
    });

    const error = screen.getByText(
      'Mithril partial sync is disabled by launcher configuration.'
    );

    expect(error).toBeInTheDocument();
    expect(error).toHaveClass('mithrilPartialSyncConfirmationError');
  });
});
