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

// The error of a rebuild that could not delete the old blockchain data. The
// rebuild recorded itself first, so the watchdog reports it as pending, but
// the data was not removed.
const WIPE_FAILED_CODE = 'WIPE_FAILED';

// Codes of the refusals of a rebuild that come before it deletes anything.
// The watchdog then starts the node again on the chain it had, so the error
// view's action that starts the node continues with that chain. A refusal
// that comes after the delete leaves no chain, and the view offers the sync
// choice instead.
const CHAIN_KEPT_REBUILD_CODES: Array<string> = [
  'ANCILLARY_VKEY_MISSING',
  'CHAIN_DIR_OTHER_NETWORK',
  'INVALID_CHAIN_PATH',
  'PROBE_FAILED',
  'CHAIN_DIR_NOT_REPLACEABLE',
  'STAGING_FAILED',
  'INSUFFICIENT_SPACE',
  'REBUILD_MARKER_FAILED',
];

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
    const { loadingPhase, mithrilProgress, lastError, lastErrorCode } = backend;
    // A confirmed rebuild that the watchdog has not reported yet shows as
    // preparing, not as the error it replaces
    const rebuildStarting = backend.rebuildStarting;
    const mithrilPhase = rebuildStarting ? 'preparing' : backend.mithrilPhase;
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
    const canReplayLedger = lastErrorCode === 'PARTIAL_SYNC_CONVERSION_FAILED';
    // Without a chain, skipping Mithril starts a sync from genesis, which the
    // user must choose knowingly: the sync choice names it, Skip does not.
    // This covers a refused first bootstrap and a rebuild refused in a new
    // location as well as removed data.
    const noChain =
      backend.hasChain === false || chainRemoved || installUnfinished;
    const nodeRestartedOnChain =
      backend.lastErrorRebuild &&
      lastErrorCode != null &&
      CHAIN_KEPT_REBUILD_CODES.includes(lastErrorCode);
    const errorActions: Array<ErrorAction> = [
      {
        label: intl.formatMessage(messages.errorRetry),
        onClick: () => backend.retryMithril(),
        variant: 'primary',
      },
    ];
    if (canReplayLedger) {
      errorActions[0].variant = 'secondary';
      errorActions.unshift({
        label: intl.formatMessage(messages.errorReplayLedger),
        onClick: () => backend.replayMithrilLedger(),
        variant: 'primary',
      });
    }
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
              label: intl.formatMessage(
                nodeRestartedOnChain ? messages.errorNotNow : messages.errorSkip
              ),
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
            chainRemoved={chainRemoved && lastErrorCode !== WIPE_FAILED_CODE}
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
            hint={
              canReplayLedger
                ? intl.formatMessage(messages.errorReplayLedgerHint)
                : undefined
            }
            error={
              lastError
                ? { code: lastErrorCode ?? 'mithril_error', message: lastError }
                : null
            }
            rebuild={backend.lastErrorRebuild}
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
          // There is no run to cancel until the watchdog starts it
          hideAction={rebuildStarting}
          onAction={() => backend.cancelMithril()}
        />
      </div>
    );
  }
}

export default MithrilSyncContainer;
