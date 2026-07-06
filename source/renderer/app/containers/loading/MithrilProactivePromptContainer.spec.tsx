import React from 'react';
import { IntlProvider } from 'react-intl';
import { cleanup, render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';

import translations from '../../i18n/locales/en-US.json';
import MithrilProactivePromptContainer from './MithrilProactivePromptContainer';

// Renders the real SyncingConnectingMithrilPrompt so the gating matrix is asserted against displayed copy, not an internal flag.

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
    // The shared fixture keeps tips finite (network 100, local 97 → 3 epochs) so computeBehindByEpochs returns 3. isSignificantlyBehind=true is the sole gate signal; the anti-flash tests verify isBehindnessKnown no longer gates independently.
    networkTip: { epoch: 100 },
    localTip: { epoch: 97 },
    isConnected: true,
    isBehindnessKnown: true,
    ...networkStatus,
  },
  mithrilPartialSync: {
    status: 'idle',
    isPartialSyncEnabled: true,
    isSignificantlyBehind: true,
    proactivePromptDismissedThisSession: false,
    // re-pop guard + beacon anchor default to the
    // "no attempt yet / no certified epoch" state so the networkTip cases above
    // are unaffected.
    mithrilAttemptStartedThisSession: false,
    certifiedEpoch: undefined,
    startPartialSync: jest.fn(() => Promise.resolve()),
    dismissProactivePrompt: jest.fn(),
    ...mithrilPartialSync,
  },
});

const renderContainer = (overrides: StoreOverrides = {}) =>
  render(
    <IntlProvider locale="en-US" messages={translations}>
      <MithrilProactivePromptContainer stores={makeStores(overrides) as any} />
    </IntlProvider>
  );

describe('MithrilProactivePromptContainer', () => {
  afterEach(cleanup);

  it('renders the prompt with the displayed epochs figure when every gate passes', () => {
    renderContainer();

    expect(
      screen.getByRole('heading', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(screen.getByText(KNOWN_EPOCHS_TEXT)).toBeInTheDocument();
    expect(screen.getByText('Note:')).toBeInTheDocument();
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

  it('renders nothing while the node is not yet loaded (isConnected false — node-loaded gate)', () => {
    const { container } = renderContainer({
      networkStatus: { isConnected: false },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders the prompt with generic behind text when isSignificantlyBehind is true but epoch diff is zero', () => {
    // isSignificantlyBehind is the definitive gate — it confirms an immutable-file gap even when epoch
    // math resolves to zero (both tips at the same epoch). computeBehindByEpochs returns undefined,
    // so the component falls back to "Your node is behind the blockchain tip." rather than hiding.
    renderContainer({
      networkStatus: { networkTip: { epoch: 97 }, localTip: { epoch: 97 } },
    });

    expect(
      screen.getByRole('heading', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(
      screen.getByText('Your node is behind the blockchain tip.')
    ).toBeInTheDocument();
    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
  });

  it('renders nothing once a Mithril attempt has begun this session (re-pop guard)', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { mithrilAttemptStartedThisSession: true },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders during early sync via the certified-beacon epoch when networkTip is null', () => {
    // networkTip unresolved (isBehindnessKnown false) but a finite certifiedEpoch > localTip.epoch makes behind-ness known via the beacon, so the prompt appears with the certified figure (105 − 97 = 8).
    renderContainer({
      networkStatus: {
        networkTip: null,
        localTip: { epoch: 97 },
        isBehindnessKnown: false,
      },
      mithrilPartialSync: { certifiedEpoch: 105 },
    });

    expect(
      screen.getByText('Your node is about 8 epochs behind.')
    ).toBeInTheDocument();
  });

  it('prefers networkTip.epoch over certifiedEpoch when both are finite', () => {
    // networkTip 100 vs certifiedEpoch 105, local 97: the hybrid anchor prefers
    // the live networkTip -> 100 - 97 = 3, NOT the certified diff (105 - 97 = 8).
    renderContainer({
      mithrilPartialSync: { certifiedEpoch: 105 },
    });

    expect(screen.getByText(KNOWN_EPOCHS_TEXT)).toBeInTheDocument();
    expect(
      screen.queryByText('Your node is about 8 epochs behind.')
    ).not.toBeInTheDocument();
  });

  it('renders the prompt even when isBehindnessKnown is false — isSignificantlyBehind is the sole anti-flash guard', () => {
    // The backend probe, not epoch math, is the offer signal: isSignificantlyBehind starts false and
    // becomes true only after the probe confirms an immutable-file gap. isBehindnessKnown being false
    // (networkTip epoch not yet resolved) no longer blocks the prompt.
    renderContainer({
      networkStatus: { isBehindnessKnown: false },
    });

    expect(
      screen.getByRole('heading', { name: 'Mithril Sync' })
    ).toBeInTheDocument();
    expect(screen.getByText(KNOWN_EPOCHS_TEXT)).toBeInTheDocument();
  });

  it('renders nothing once the prompt has been dismissed for the session', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { proactivePromptDismissedThisSession: true },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });
});
