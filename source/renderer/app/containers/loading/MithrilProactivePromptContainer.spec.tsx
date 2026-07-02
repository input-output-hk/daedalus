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
    // what the anti-flash test below exercises. `isConnected` is the node-loaded
    // gate.
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

  it('renders nothing while the node is not yet loaded (isConnected false — node-loaded gate)', () => {
    // Everything else passes (finite tips, behind-ness known), but the node is
    // still connecting / verifying the blockchain: the prompt must stay hidden.
    const { container } = renderContainer({
      networkStatus: { isConnected: false },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing near the tip (behind-by <= 0 -> undefined) even though behind-ness is known', () => {
    // Equal epochs -> computeBehindByEpochs returns undefined (near-tip hide),
    // although isBehindnessKnown is still true (both tips finite).
    const { container } = renderContainer({
      networkStatus: { networkTip: { epoch: 97 }, localTip: { epoch: 97 } },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders nothing once a Mithril attempt has begun this session (re-pop guard)', () => {
    const { container } = renderContainer({
      mithrilPartialSync: { mithrilAttemptStartedThisSession: true },
    });

    expect(screen.queryByText(KNOWN_EPOCHS_TEXT)).not.toBeInTheDocument();
    expect(container).toBeEmptyDOMElement();
  });

  it('renders during early sync via the certified-beacon epoch when networkTip is null', () => {
    // networkTip not yet resolved (isBehindnessKnown false), but a finite
    // certifiedEpoch > localTip.epoch means behind-ness IS known via the beacon:
    // the prompt now appears (certified-anchored figure) instead of staying
    // suppressed — the core defect fix. 105 - 97 = 8 epochs.
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

  it('renders nothing until behind-ness is KNOWN — gates on networkStatus.isBehindnessKnown, NOT a local tip re-derivation (anti-flash)', () => {
    // Tips ARE present + finite (so a local `behindByEpochs !== undefined`
    // re-derivation would be true), but `isBehindnessKnown` is still
    // false: the prompt must stay hidden. This proves the gate consumes the
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
