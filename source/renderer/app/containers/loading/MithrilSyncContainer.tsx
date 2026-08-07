import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import MithrilDecisionView from '../../components/loading/mithril/MithrilDecisionView';
import MithrilProgressView from '../../components/loading/mithril/MithrilProgressView';
import MithrilErrorView from '../../components/loading/mithril/MithrilErrorView';
import styles from '../../components/loading/mithril/MithrilBootstrap.scss';

const TERMINAL_PHASES = new Set(['completed', 'cancelled', 'error']);

@inject('stores', 'actions')
@observer
class MithrilSyncContainer extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  _mithrilStartedAt: number | null = null;

  render() {
    const { backend, app } = this.props.stores;
    const { loadingPhase, mithrilPhase, mithrilProgress, lastError } = backend;

    // Track wall-clock start time for elapsed timer; reset on terminal phases.
    if (mithrilPhase && !TERMINAL_PHASES.has(mithrilPhase)) {
      if (this._mithrilStartedAt === null) {
        this._mithrilStartedAt = Date.now();
      }
    } else {
      this._mithrilStartedAt = null;
    }

    // 'verifying' mithrilPhase == ledger/ancillary download; 'downloading' == snapshot files.
    // Route bytes to ancillaryBytes* during verifying so inLedgerPhase fires in
    // MithrilStepIndicator (bypassing isVerificationOrLater → 100% pin).
    const isLedgerPhase = mithrilPhase === 'verifying';

    if (loadingPhase === 'bootstrap-decision') {
      return (
        <div className={styles.card}>
          <MithrilDecisionView
            onAccept={() => backend.startMithril()}
            onDecline={() => backend.startNode()}
          />
        </div>
      );
    }

    if (mithrilPhase === 'error') {
      return (
        <div className={styles.card}>
          <MithrilErrorView
            error={
              lastError ? { code: 'mithril_error', message: lastError } : null
            }
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
        </div>
      );
    }

    return (
      <div className={styles.card}>
        <MithrilProgressView
          status={mithrilPhase ?? 'preparing'}
          filesDownloaded={mithrilProgress?.filesDownloaded}
          filesTotal={mithrilProgress?.filesTotal}
          snapshotBytesDownloaded={
            isLedgerPhase ? undefined : mithrilProgress?.bytesDownloaded
          }
          snapshotBytesTotal={
            isLedgerPhase ? undefined : mithrilProgress?.bytesTotal
          }
          ancillaryBytesDownloaded={
            isLedgerPhase ? mithrilProgress?.bytesDownloaded : undefined
          }
          ancillaryBytesTotal={
            isLedgerPhase ? mithrilProgress?.bytesTotal : undefined
          }
          bootstrapStartedAt={this._mithrilStartedAt}
          showDownloadProgressBar
          onAction={() => backend.cancelMithril()}
        />
      </div>
    );
  }
}

export default MithrilSyncContainer;
