import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import SyncingConnectingMithrilPrompt from '../../components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';

type Props = InjectedProps;

/**
 * App-level owner of the proactive Mithril prompt (D-702a-5 / CAT-F).
 *
 * Why this container exists (it replaces the old loading-screen mount):
 * - PERSIST across loading -> Wallet Summary: the prompt used to be mounted inside
 *   the loading screen (`SyncingConnecting.tsx`), which unmounts the moment Root
 *   routes the app in, so the prompt vanished on that transition (ISSUE-7). Mounting
 *   here (via `App.tsx`, a sibling of `<Router>`) keeps it alive across every route
 *   until the user picks Standard Sync or Mithril Sync.
 * - ANTI-FLASH known-behind gate: gate on CAT-A's reactive
 *   `networkStatus.isBehindnessKnown` computed (true only once both tips carry finite
 *   epochs). This suppresses the early flash during the connecting / "verifying
 *   blockchain" checks when behind-ness is not yet known. The gate consumes CAT-A's
 *   computed DIRECTLY — it does NOT re-derive `behindByEpochs !== undefined` — so that
 *   deliverable stays load-bearing; `behindByEpochs` here feeds only the displayed
 *   figure.
 * - MUTUAL EXCLUSION with the partial-sync overlay: gate on
 *   `mithrilPartialSync.status === 'idle'`. Once "Start now" flips the status to a
 *   working/overlay status the prompt returns `null` and the App-level
 *   `MithrilPartialSyncOverlay` takes over, so the two never co-render. During the
 *   empty-chain Mithril bootstrap there is no local tip and status !== 'idle', so the
 *   prompt stays hidden (#11).
 */
@inject('stores', 'actions')
@observer
class MithrilProactivePromptContainer extends Component<Props> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const { networkStatus, mithrilPartialSync } = this.props.stores;
    const { networkTip, localTip, isBehindnessKnown } = networkStatus;

    // Displayed figure only (ported verbatim from SyncingConnectingPage). The
    // known-gate below is CAT-A's `isBehindnessKnown`, NOT this local derivation.
    const networkEpoch =
      networkTip && Number.isFinite(networkTip.epoch) ? networkTip.epoch : null;
    const localEpoch =
      localTip && Number.isFinite(localTip.epoch) ? localTip.epoch : null;
    const behindByEpochs =
      networkEpoch !== null && localEpoch !== null
        ? Math.max(1, networkEpoch - localEpoch)
        : undefined;

    const showMithrilPrompt =
      mithrilPartialSync.status === 'idle' &&
      mithrilPartialSync.isPartialSyncEnabled &&
      mithrilPartialSync.isSignificantlyBehind &&
      isBehindnessKnown &&
      !mithrilPartialSync.proactivePromptDismissedThisSession;

    if (!showMithrilPrompt) {
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
