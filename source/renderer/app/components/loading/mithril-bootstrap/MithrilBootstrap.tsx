import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import type {
  MithrilBootstrapStatus,
  MithrilSnapshotItem,
  MithrilBootstrapError,
  ChainStorageValidation,
} from '../../../../../common/types/mithril-bootstrap.types';
import messages from './MithrilBootstrap.messages';
import MithrilDecisionView from './MithrilDecisionView';
import MithrilErrorView from './MithrilErrorView';
import MithrilProgressView from './MithrilProgressView';
import styles from './MithrilBootstrap.scss';

interface Props {
  status: MithrilBootstrapStatus;
  progress: number;
  currentStep?: string;
  filesDownloaded?: number;
  filesTotal?: number;
  bytesDownloaded?: number;
  snapshotSize?: number;
  throughputBps?: number;
  elapsedSeconds?: number;
  remainingSeconds?: number;
  customChainPath?: string | null;
  chainStorageValidation?: ChainStorageValidation;
  isChainStorageLoading?: boolean;
  storageLocationConfirmed?: boolean;
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  error?: MithrilBootstrapError | null;
  isFetchingSnapshots: boolean;
  onOpenExternalLink?: (...args: [string]) => void;
  onSetChainStorageDirectory?: (...args: [string | null]) => Promise<unknown>;
  onResetChainStorageDirectory?(): Promise<unknown>;
  onConfirmStorageLocation?(): void;
  onLoadChainStorageConfig?(): Promise<void>;
  onSelectSnapshot: (...args: [string | null]) => void;
  onAccept(): void;
  onDecline(): void;
  onWipeRetry(): void;
  onCancel(): void;
}

class MithrilBootstrap extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  renderDecision() {
    const {
      snapshots,
      selectedDigest,
      selectedSnapshot,
      isFetchingSnapshots,
      onSelectSnapshot,
      onAccept,
      onDecline,
    } = this.props;
    return (
      <div className={styles.card}>
        <MithrilDecisionView
          snapshots={snapshots}
          selectedDigest={selectedDigest}
          selectedSnapshot={selectedSnapshot}
          isFetchingSnapshots={isFetchingSnapshots}
          onSelectSnapshot={onSelectSnapshot}
          onAccept={onAccept}
          onDecline={onDecline}
        />
      </div>
    );
  }

  renderProgress() {
    const { intl } = this.context;
    const {
      status,
      progress,
      currentStep,
      bytesDownloaded,
      snapshotSize,
      throughputBps,
      elapsedSeconds,
      remainingSeconds,
      onCancel,
    } = this.props;
    return (
      <div className={styles.card}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.title)}</h1>
          <p>{currentStep || intl.formatMessage(messages.progressLabel)}</p>
        </div>
        <MithrilProgressView
          status={status}
          progress={progress}
          bytesDownloaded={bytesDownloaded}
          snapshotSize={snapshotSize}
          throughputBps={throughputBps}
          elapsedSeconds={elapsedSeconds}
          remainingSeconds={remainingSeconds}
          onCancel={onCancel}
        />
      </div>
    );
  }

  renderError() {
    const { error, onOpenExternalLink, onWipeRetry, onDecline } = this.props;
    return (
      <div className={styles.card}>
        <MithrilErrorView
          error={error}
          onOpenExternalLink={onOpenExternalLink}
          onWipeRetry={onWipeRetry}
          onDecline={onDecline}
        />
      </div>
    );
  }

  render() {
    const { status } = this.props;
    const isDecision =
      status === 'decision' || status === 'idle' || status === 'cancelled';
    const isWorking =
      status === 'preparing' ||
      status === 'downloading' ||
      status === 'installing' ||
      status === 'finalizing' ||
      status === 'verifying' ||
      status === 'converting';
    const isError = status === 'failed';
    return (
      <div className={styles.component}>
        <div className={styles.backdrop} />
        <div className={styles.content}>
          {isDecision && this.renderDecision()}
          {isWorking && this.renderProgress()}
          {isError && this.renderError()}
        </div>
      </div>
    );
  }
}

export default observer(MithrilBootstrap);
