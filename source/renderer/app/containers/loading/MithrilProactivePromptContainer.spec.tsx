import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

import translations from '../../i18n/locales/en-US.json';
import MithrilProactivePromptContainer from './MithrilProactivePromptContainer';

// The container renders the real SyncingConnectingMithrilPrompt so the gating
// matrix is asserted against the actually-displayed copy ("about 3 epochs
// behind") rather than an internal flag.

const KNOWN_EPOCHS_TEXT = 'Your node is about 3 epochs behind.';

type StoreOverrides = {
  networkStatus?: Record<string, unknown>;
  mithrilPartialSync?: Record<string, unknown>;
};

const makeStores = ({
  networkStatus = {},
  mithrilPartialSync = {},
}: StoreOverrides = {}) => ({
  networkStatus: {
    // Tips are ALWAYS present + finite here (network 100, local 97 -> 3 epochs),
    // so a (wrong) local `behindByEpochs !== undefined` re-derivation would be
    // truthy in every case. Only `isBehindnessKnown` gates the prompt, which is
    // what the anti-flash test below exercises.
    networkTip: { epoch: 100 },
    localTip: { epoch: 97 },
    isBehindnessKnown: true,
    ...networkStatus,
  },
  mithrilPartialSync: {
    status: 'idle',
    isPartialSyncEnabled: true,
    isSignificantlyBehind: true,
    proactivePromptDismissedThisSession: false,
    startPartialSync: jest.fn(() => Promise.resolve()),
    dismissProactivePrompt: jest.fn(),
    ...mithrilPartialSync,
  },
});

const renderContainer = (overrides: StoreOverrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <MithrilProactivePromptContainer
        stores={makeStores(overrides) as any}
        actions={{} as any}
      />
    </IntlProvider>
  );

describe('MithrilProactivePromptContainer', () => {
  afterEach(cleanup);

  it('renders the prompt with the displayed epochs figure when every gate passes', () => {
    renderContainer();

    expect(screen.getByText(KNOWN_EPOCHS_TEXT)).toBeInTheDocument();
    // The persisted-prompt choice view exposes both actions.
    expect(
      screen.getByRole('button', { name: 'Mithril Sync (fast)' })
    ).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: 'Standard Sync (slow)' })
    ).toBeInTheDocument();
  });

  it('renders nothing while a Mithril sync is already in flight (status !== idle)', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { status: 'downloading' },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing when partial sync is disabled (kill switch off)', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { isPartialSyncEnabled: false },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing when the node is not significantly behind', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { isSignificantlyBehind: false },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing until behind-ness is KNOWN — gates on networkStatus.isBehindnessKnown, NOT a local tip re-derivation (anti-flash)', () => {
    // Tips ARE present + finite (so a local `behindByEpochs !== undefined`
    // re-derivation would be true), but CAT-A's `isBehindnessKnown` is still
    // false: the prompt must stay hidden. This proves the gate consumes CAT-A's
    // computed signal and never flashes during the early connecting / verifying
    // checks.
    const { container } = renderContainer({
      networkStatus: { isBehindnessKnown: false },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing once the prompt has been dismissed for the session', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { proactivePromptDismissedThisSession: true },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });
});
