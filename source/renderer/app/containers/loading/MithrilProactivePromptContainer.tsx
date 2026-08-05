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
    const { networkStatus, mithrilSync } = this.props.stores;
    const { localTip, networkTip } = networkStatus;
    const { certifiedEpoch } = mithrilSync; // early-sync beacon anchor

    const { isConnected } = networkStatus;

    const isGated =
      mithrilSync.status === 'idle' &&
      mithrilSync.isSignificantlyBehind && // watchdog probe signal; false until first event received
      isConnected &&
      !mithrilSync.mithrilAttemptStartedThisSession && // re-pop guard
      !mithrilSync.proactivePromptDismissedThisSession;

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
        onStart={mithrilSync.startPartialSync}
        onDismiss={mithrilSync.dismissProactivePrompt}
      />
    );
  }
}

export default MithrilProactivePromptContainer;
