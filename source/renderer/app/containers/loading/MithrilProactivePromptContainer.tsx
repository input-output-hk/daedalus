import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedStoresProps } from '../../types/injectedPropsType';
import SyncingConnectingMithrilPrompt from '../../components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
import { computeBehindByEpochs } from '../../utils/mithrilBehindness';

type Props = InjectedStoresProps;

/**
 * App-level owner of the proactive Mithril prompt; mounted from App.tsx (not the
 * loading screen) so it survives the loading -> Wallet Summary route change. The
 * render gate below decides when it is offered.
 */
@inject('stores')
@observer
class MithrilProactivePromptContainer extends Component<Props> {
  static defaultProps = {
    stores: null,
  };

  render() {
    const { networkStatus, mithrilPartialSync } = this.props.stores;
    const { localTip, networkTip, isConnected, isBehindnessKnown } =
      networkStatus;
    const { certifiedEpoch } = mithrilPartialSync; // early-sync beacon anchor

    // Broadens the networkTip-only isBehindnessKnown so a known-behind state is recognised early via the certified epoch too.
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

    // Computed after the gate so it runs only when the prompt renders; undefined here also hides the prompt (node at/ahead of the anchor).
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
