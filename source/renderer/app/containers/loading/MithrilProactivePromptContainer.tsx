import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedStoresProps } from '../../types/injectedPropsType';
import SyncingConnectingMithrilPrompt from '../../components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
import { computeBehindByEpochs } from '../../utils/mithrilBehindness';

type Props = InjectedStoresProps;

/**
 * App-level owner of the proactive Mithril prompt.
 *
 * Why this container exists (it replaces the old loading-screen mount):
 * - PERSIST across loading -> Wallet Summary: the prompt used to be mounted inside
 *   the loading screen (`SyncingConnecting.tsx`), which unmounts the moment Root
 *   routes the app in, so the prompt vanished on that transition. Mounting
 *   here (via `App.tsx`, a sibling of `<Router>`) keeps it alive across every route
 *   until the user picks Standard Sync or Mithril Sync — KEEPS this
 *   cross-screen persistence; it is NOT route-scoped.
 * - NODE-LOADED trigger: gate on `networkStatus.isConnected` so the prompt
 *   appears only once the Cardano node is loaded (past the early connecting /
 *   "verifying blockchain" checks — `isVerifyingBlockchain` is `!isConnected && …`),
 *   never flashing during them.
 * - ANTI-FLASH known-behind gate: gate on the reactive
 *   `networkStatus.isBehindnessKnown` computed (true only once both tips carry finite
 *   epochs), broadened with `certifiedKnown` OR (finite local epoch AND finite
 *   certified-beacon epoch) so a known-behind state is recognised early in sync too.
 *   This cheap named boolean short-circuits BEFORE `computeBehindByEpochs` is
 *   evaluated (observer-perf), so the figure is computed only when the prompt
 *   actually renders.
 * - NEAR-TIP hide: after the gate, a SEPARATE early-return hides the
 *   prompt when `computeBehindByEpochs(...) === undefined` (node level with / ahead
 *   of the anchor), eliminating the misleading "about 1 epochs behind" at the source.
 * - RE-POP guard: AND-in `!mithrilAttemptStartedThisSession` so once
 *   a Mithril attempt has begun this session the prompt never re-offers, regardless
 *   of the terminal outcome.
 * - MUTUAL EXCLUSION with the partial-sync overlay: gate on
 *   `mithrilPartialSync.status === 'idle'`. Once "Start now" flips the status to a
 *   working/overlay status the prompt returns `null` and the App-level
 *   `MithrilPartialSyncOverlay` takes over, so the two never co-render. During the
 *   empty-chain Mithril bootstrap there is no local tip and status !== 'idle', so the
 *   prompt stays hidden.
 */
@inject('stores')
@observer
class MithrilProactivePromptContainer extends Component<Props> {
  static defaultProps = {
    stores: null,
  };

  render() {
    const { networkStatus, mithrilPartialSync } = this.props.stores;
    const {
      localTip,
      networkTip,
      isConnected,
      isBehindnessKnown,
    } = networkStatus;
    const { certifiedEpoch } = mithrilPartialSync; // early-sync beacon anchor

    // combined known-ness = local epoch finite AND (live network tip finite OR
    // certified epoch finite). The named `NetworkStatusStore.isBehindnessKnown` stays
    // the networkTip sub-signal; the certified OR is composed HERE so the util/gate
    // decoupling holds.
    const certifiedKnown =
      Number.isFinite(localTip?.epoch) && Number.isFinite(certifiedEpoch);

    const isGated =
      mithrilPartialSync.status === 'idle' &&
      mithrilPartialSync.isPartialSyncEnabled &&
      mithrilPartialSync.isSignificantlyBehind && // backend offer signal (near-tip ⇒ false)
      isConnected && // node loaded, past verifying
      (isBehindnessKnown || certifiedKnown) && // anti-flash known-gate, now beacon-aware
      !mithrilPartialSync.mithrilAttemptStartedThisSession && // re-pop guard
      !mithrilPartialSync.proactivePromptDismissedThisSession;

    if (!isGated) {
      return null;
    }

    // Display figure + near-tip hide: undefined when local >= chosen
    // anchor. Hybrid: prefer `networkTip.epoch` when finite, else `certifiedEpoch`.
    // Computed AFTER the cheap gate so it never runs when the prompt is hidden.
    const behindByEpochs = computeBehindByEpochs(
      localTip,
      networkTip,
      certifiedEpoch
    );
    if (behindByEpochs === undefined) {
      return null;
    }

    return (
      <SyncingConnectingMithrilPrompt
        behindByEpochs={behindByEpochs}
        onStart={mithrilPartialSync.startPartialSync}
        onDismiss={mithrilPartialSync.dismissProactivePrompt}
      />
    );
  }
}

export default MithrilProactivePromptContainer;
