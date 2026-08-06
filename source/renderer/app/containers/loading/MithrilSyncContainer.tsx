import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import MithrilDecisionView from '../../components/loading/mithril/MithrilDecisionView';
import MithrilProgressView from '../../components/loading/mithril/MithrilProgressView';
import MithrilErrorView from '../../components/loading/mithril/MithrilErrorView';

@inject('stores', 'actions')
@observer
class MithrilSyncContainer extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const { backend, app } = this.props.stores;
    const { loadingPhase, mithrilPhase, mithrilProgress, lastError } = backend;

    if (loadingPhase === 'bootstrap-decision') {
      return (
        <MithrilDecisionView
          onAccept={() => backend.startMithril()}
          onDecline={() => backend.startNode()}
        />
      );
    }

    if (mithrilPhase === 'error') {
      return (
        <MithrilErrorView
          error={lastError ? { code: 'mithril_error', message: lastError } : null}
          onOpenExternalLink={(url) => app.openExternalLink(url)}
          actions={[
            {
              label: 'Retry',
              onClick: () => backend.startMithril(),
              variant: 'primary' as const,
            },
            {
              label: 'Skip to blockchain sync',
              onClick: () => backend.startNode(),
              variant: 'secondary' as const,
            },
          ]}
        />
      );
    }

    return (
      <MithrilProgressView
        status={mithrilPhase ?? 'preparing'}
        filesDownloaded={mithrilProgress?.filesDownloaded}
        filesTotal={mithrilProgress?.filesTotal}
        snapshotBytesDownloaded={mithrilProgress?.bytesDownloaded}
        snapshotBytesTotal={mithrilProgress?.bytesTotal}
        elapsedSeconds={mithrilProgress?.secondsElapsed}
        showDownloadProgressBar
        onAction={() => backend.cancelMithril()}
      />
    );
  }
}

export default MithrilSyncContainer;
