import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import MithrilDecisionView from '../../components/loading/mithril/MithrilDecisionView';
import MithrilProgressView from '../../components/loading/mithril/MithrilProgressView';
import RebuildConfirmView from '../../components/loading/chain-damage/RebuildConfirmView';
import MithrilErrorView from '../../components/loading/mithril/MithrilErrorView';
import messages from '../../components/loading/mithril/MithrilBootstrap.messages';
import styles from '../../components/loading/mithril/MithrilBootstrap.scss';

const TERMINAL_PHASES = new Set(['completed', 'cancelled', 'error']);

type ErrorAction = {
  label: string;
  onClick: () => void;
  variant: 'primary' | 'secondary';
};

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
    // A rebuild deleted the chain data and has not finished: there is nothing
    // to skip to, since the node would start a sync from genesis.
    const chainRemoved = backend.rebuildPending;
    // A Mithril install began changing the chain data and did not finish, e.g.
    // a first download cut short; nothing old was removed, and there is no
    // chain to skip to either.
    const installUnfinished = !chainRemoved && backend.installPending;
    // After a failed install the watchdog starts no node until a Mithril sync
    // installs a database, so Retry is the only action.
    const retryOnly = backend.installRetryRequired;
    // Without a chain, skipping Mithril starts a sync from genesis, which the
    // user must choose knowingly: the sync choice names it, Skip does not.
    // This covers a refused first bootstrap and a rebuild refused in a new
    // location as well as removed data.
    const noChain =
      backend.hasChain === false || chainRemoved || installUnfinished;
    const errorActions: Array<ErrorAction> = [
      {
        label: intl.formatMessage(messages.errorRetry),
        onClick: () => backend.retryMithril(),
        variant: 'primary',
      },
    ];
    if (backend.canChooseAnotherLocation) {
      errorActions.push({
        label: intl.formatMessage(messages.errorChooseLocation),
        onClick: () => backend.chooseAnotherLocation(),
        variant: 'secondary',
      });
    }
    if (!retryOnly) {
      errorActions.push(
        noChain
          ? {
              label: intl.formatMessage(messages.errorChooseSync),
              onClick: () => backend.showBootstrapDecision(),
              variant: 'secondary',
            }
          : {
              label: intl.formatMessage(messages.errorSkip),
              onClick: () => backend.startNode(),
              variant: 'secondary',
            }
      );
    }

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
            chainRemoved={chainRemoved}
            installUnfinished={installUnfinished}
            onAccept={() => backend.startMithril()}
            onDecline={() => backend.startNode()}
          />
        </div>
      );
    }

    if (mithrilPhase === 'error' && backend._rebuildConfirmationRequested) {
      return (
        <div className={styles.card}>
          <RebuildConfirmView
            location={backend.pendingChainLocation}
            onRebuild={() => backend.rebuildChain()}
            onBack={() => backend.cancelRebuildConfirmation()}
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
            installUnfinished={installUnfinished}
            retryOnly={retryOnly}
            onOpenExternalLink={(url) => app.openExternalLink(url)}
            actions={errorActions}
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
          waitingForNetwork={backend.mithrilWaitingForNetwork}
          onAction={() => backend.cancelMithril()}
        />
      </div>
    );
  }
}

export default MithrilSyncContainer;
