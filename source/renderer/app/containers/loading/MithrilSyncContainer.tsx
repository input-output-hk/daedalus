import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import MithrilDecisionView from '../../components/loading/mithril/MithrilDecisionView';
import MithrilProgressView from '../../components/loading/mithril/MithrilProgressView';
import MithrilErrorView from '../../components/loading/mithril/MithrilErrorView';
import messages from '../../components/loading/mithril/MithrilBootstrap.messages';
import styles from '../../components/loading/mithril/MithrilBootstrap.scss';

const TERMINAL_PHASES = new Set(['completed', 'cancelled', 'error']);

@inject('stores', 'actions')
@observer
class MithrilSyncContainer extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  _mithrilStartedAt: number | null = null;

  render() {
    const { backend, app } = this.props.stores;
    const {
      loadingPhase,
      mithrilPhase,
      mithrilProgress,
      lastError,
      lastErrorCode,
    } = backend;
    const { intl } = this.context;
    // A failed rebuild that had already deleted the chain data leaves nothing
    // to skip to: the node would start a sync from genesis.
    const chainRemoved = backend.lastErrorRebuild && backend.hasChain === false;

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
            chainRemoved={backend.rebuildPending || chainRemoved}
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
              lastError
                ? { code: lastErrorCode ?? 'mithril_error', message: lastError }
                : null
            }
            chainRemoved={chainRemoved}
            onOpenExternalLink={(url) => app.openExternalLink(url)}
            actions={[
              {
                label: intl.formatMessage(messages.errorRetry),
                onClick: () => backend.retryMithril(),
                variant: 'primary' as const,
              },
              chainRemoved
                ? {
                    label: intl.formatMessage(messages.errorChooseSync),
                    onClick: () => backend.showBootstrapDecision(),
                    variant: 'secondary' as const,
                  }
                : {
                    label: intl.formatMessage(messages.errorSkip),
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
