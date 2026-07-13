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
    const { localTip, networkTip, isConnected } = networkStatus;
    const { certifiedEpoch } = mithrilPartialSync; // early-sync beacon anchor

    // isSignificantlyBehind is the definitive backend signal (immutable-file gap ≥ threshold);
    // it starts false and only becomes true after the probe confirms the gap, so it provides
    // anti-flash protection on its own. The separate epoch-based gate was redundant and blocked
    // the prompt when certifiedEpoch was absent or below localTip.epoch (semi-recent data).
    const isGated =
      mithrilPartialSync.status === 'idle' &&
      mithrilPartialSync.isPartialSyncEnabled &&
      mithrilPartialSync.isSignificantlyBehind && // backend offer signal (near-tip ⇒ false)
      isConnected && // node loaded, past verifying
      !mithrilPartialSync.mithrilAttemptStartedThisSession && // re-pop guard
      !mithrilPartialSync.proactivePromptDismissedThisSession;

    if (!isGated) {
      return null;
    }

    // Computed after the gate so it runs only when the prompt renders. undefined means no
    // epoch anchor is available yet; the component falls back to "Your node is behind the
    // blockchain tip." rather than hiding — isSignificantlyBehind already confirmed the gap.
    const behindByEpochs = computeBehindByEpochs(
      localTip,
      networkTip,
      certifiedEpoch
    );

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
