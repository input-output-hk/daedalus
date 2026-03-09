import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import ProgressBarLarge from '../../widgets/ProgressBarLarge';
import { formattedBytesToSize } from '../../../utils/formatters';
import type {
  MithrilBootstrapStatus,
  MithrilSnapshotItem,
  MithrilBootstrapError,
} from '../../../../../common/types/mithril-bootstrap.types';
import styles from './MithrilBootstrap.scss';

const messages = defineMessages({
  title: {
    id: 'loading.mithrilBootstrap.title',
    defaultMessage: '!!!Fast sync with Mithril',
    description: 'Headline for Mithril bootstrap prompt.',
  },
  description: {
    id: 'loading.mithrilBootstrap.description',
    defaultMessage:
      '!!!Mithril can download a verified snapshot to sync your wallet faster. Choose a snapshot and continue, or sync from genesis.',
    description: 'Description for Mithril bootstrap prompt.',
  },
  accept: {
    id: 'loading.mithrilBootstrap.accept',
    defaultMessage: '!!!Use Mithril fast sync',
    description: 'Button label to accept Mithril bootstrap.',
  },
  decline: {
    id: 'loading.mithrilBootstrap.decline',
    defaultMessage: '!!!Sync from genesis',
    description: 'Button label to decline Mithril bootstrap.',
  },
  selectLabel: {
    id: 'loading.mithrilBootstrap.selectLabel',
    defaultMessage: '!!!Snapshot',
    description: 'Label for snapshot selector.',
  },
  snapshotLatest: {
    id: 'loading.mithrilBootstrap.snapshotLatest',
    defaultMessage: '!!!Latest snapshot',
    description: 'Label for latest snapshot option.',
  },
  snapshotDetailsTitle: {
    id: 'loading.mithrilBootstrap.snapshotDetailsTitle',
    defaultMessage: '!!!Snapshot details',
    description: 'Title for snapshot metadata section.',
  },
  snapshotDetailsUnavailable: {
    id: 'loading.mithrilBootstrap.snapshotDetailsUnavailable',
    defaultMessage: '!!!Snapshot details unavailable',
    description: 'Fallback text when snapshot metadata is missing.',
  },
  snapshotSizeLabel: {
    id: 'loading.mithrilBootstrap.snapshotSizeLabel',
    defaultMessage: '!!!Size',
    description: 'Label for snapshot size metadata.',
  },
  snapshotCreatedLabel: {
    id: 'loading.mithrilBootstrap.snapshotCreatedLabel',
    defaultMessage: '!!!Created',
    description: 'Label for snapshot creation date metadata.',
  },
  snapshotNodeVersionLabel: {
    id: 'loading.mithrilBootstrap.snapshotNodeVersionLabel',
    defaultMessage: '!!!Node version',
    description: 'Label for snapshot node version metadata.',
  },
  snapshotDigestLabel: {
    id: 'loading.mithrilBootstrap.snapshotDigestLabel',
    defaultMessage: '!!!Digest',
    description: 'Label for snapshot digest metadata.',
  },
  progressLabel: {
    id: 'loading.mithrilBootstrap.progressLabel',
    defaultMessage: '!!!Bootstrap progress',
    description: 'Label for Mithril progress bar.',
  },
  progressTiming: {
    id: 'loading.mithrilBootstrap.progressTiming',
    defaultMessage: '!!!Elapsed {elapsed} • Remaining {remaining}',
    description: 'Label for Mithril elapsed/remaining time row.',
  },
  progressElapsed: {
    id: 'loading.mithrilBootstrap.progressElapsed',
    defaultMessage: '!!!Elapsed {elapsed}',
    description: 'Label for Mithril elapsed time only row.',
  },
  progressRemaining: {
    id: 'loading.mithrilBootstrap.progressRemaining',
    defaultMessage: '!!!Remaining {remaining}',
    description: 'Label for Mithril remaining time only row.',
  },
  cancel: {
    id: 'loading.mithrilBootstrap.cancel',
    defaultMessage: '!!!Cancel',
    description: 'Button label to cancel Mithril bootstrap.',
  },
  wipeAndRetry: {
    id: 'loading.mithrilBootstrap.wipeAndRetry',
    defaultMessage: '!!!Wipe chain & retry',
    description: 'Button label to wipe chain and retry Mithril bootstrap.',
  },
  errorTitle: {
    id: 'loading.mithrilBootstrap.errorTitle',
    defaultMessage: '!!!Mithril bootstrap failed',
    description: 'Title for Mithril error state.',
  },
  startFailureHint: {
    id: 'loading.mithrilBootstrap.startFailureHint',
    defaultMessage:
      '!!!The node could not start with the restored chain data. Wipe the chain and try Mithril again, or sync from genesis.',
    description: 'Hint for node start failure after Mithril bootstrap.',
  },
});

type Props = {
  status: MithrilBootstrapStatus;
  progress: number;
  currentStep?: string;
  filesDownloaded?: number;
  filesTotal?: number;
  bytesDownloaded?: number;
  throughputBps?: number;
  elapsedSeconds?: number;
  remainingSeconds?: number;
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  selectedSnapshot?: MithrilSnapshotItem | null;
  error?: MithrilBootstrapError | null;
  isFetchingSnapshots: boolean;
  onSelectSnapshot: (digest: string | null) => void;
  onAccept: () => void;
  onDecline: () => void;
  onWipeRetry: () => void;
  onCancel: () => void;
};

type SnapshotOption = {
  value: string;
  label: string;
};

class MithrilBootstrap extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  buildOptions = (): Array<SnapshotOption> => {
    const { intl } = this.context;
    const { snapshots } = this.props;
    const options: Array<SnapshotOption> = [
      {
        value: 'latest',
        label: intl.formatMessage(messages.snapshotLatest),
      },
    ];
    snapshots.forEach((snapshot) => {
      const createdAt = snapshot.createdAt ? ` • ${snapshot.createdAt}` : '';
      const version = snapshot.cardanoNodeVersion
        ? ` • ${snapshot.cardanoNodeVersion}`
        : '';
      options.push({
        value: snapshot.digest,
        label: `${snapshot.digest}${createdAt}${version}`,
      });
    });
    return options;
  };

  formatSnapshotDate = (value?: string) => {
    if (!value) return null;
    const parsed = Date.parse(value);
    if (Number.isNaN(parsed)) return value;
    return new Date(parsed).toLocaleString();
  };

  renderSnapshotDetails() {
    const { intl } = this.context;
    const { selectedSnapshot } = this.props;
    if (!selectedSnapshot) {
      return (
        <div className={styles.details}>
          <div className={styles.detailsHeader}>
            {intl.formatMessage(messages.snapshotDetailsTitle)}
          </div>
          <div className={styles.detailsFallback}>
            {intl.formatMessage(messages.snapshotDetailsUnavailable)}
          </div>
        </div>
      );
    }

    const createdAt = this.formatSnapshotDate(selectedSnapshot.createdAt);
    const size =
      typeof selectedSnapshot.size === 'number' && selectedSnapshot.size > 0
        ? formattedBytesToSize(selectedSnapshot.size)
        : null;

    return (
      <div className={styles.details}>
        <div className={styles.detailsHeader}>
          {intl.formatMessage(messages.snapshotDetailsTitle)}
        </div>
        <div className={styles.detailsGrid}>
          <div className={styles.detailRow}>
            <div className={styles.detailLabel}>
              {intl.formatMessage(messages.snapshotDigestLabel)}
            </div>
            <div className={styles.detailValue}>{selectedSnapshot.digest}</div>
          </div>
          <div className={styles.detailRow}>
            <div className={styles.detailLabel}>
              {intl.formatMessage(messages.snapshotCreatedLabel)}
            </div>
            <div className={styles.detailValue}>{createdAt || 'n/a'}</div>
          </div>
          <div className={styles.detailRow}>
            <div className={styles.detailLabel}>
              {intl.formatMessage(messages.snapshotSizeLabel)}
            </div>
            <div className={styles.detailValue}>{size || 'n/a'}</div>
          </div>
          <div className={styles.detailRow}>
            <div className={styles.detailLabel}>
              {intl.formatMessage(messages.snapshotNodeVersionLabel)}
            </div>
            <div className={styles.detailValue}>
              {selectedSnapshot.cardanoNodeVersion || 'n/a'}
            </div>
          </div>
        </div>
      </div>
    );
  }

  renderDecision() {
    const { intl } = this.context;
    const {
      selectedDigest,
      isFetchingSnapshots,
      onSelectSnapshot,
      onAccept,
      onDecline,
    } = this.props;
    const options = this.buildOptions();
    const selectedValue = selectedDigest || 'latest';
    return (
      <div className={styles.card}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.title)}</h1>
          <p>{intl.formatMessage(messages.description)}</p>
        </div>
        <div className={styles.selectorRow}>
          <div className={styles.label}>
            <FormattedMessage {...messages.selectLabel} />
          </div>
          <div>
            <Select
              skin={SelectSkin}
              className={styles.selectInput}
              options={options}
              value={selectedValue}
              disabled={isFetchingSnapshots}
              onChange={(value) => onSelectSnapshot(value ?? null)}
            />
          </div>
        </div>
        {this.renderSnapshotDetails()}
        <div className={styles.actions}>
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.accept)}
            onClick={onAccept}
          />
          <Button
            className={styles.secondaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.decline)}
            onClick={onDecline}
          />
        </div>
      </div>
    );
  }

  renderProgress() {
    const { intl } = this.context;
    const {
      progress,
      currentStep,
      filesDownloaded,
      filesTotal,
      bytesDownloaded,
      throughputBps,
      elapsedSeconds,
      remainingSeconds,
      onCancel,
    } = this.props;
    const normalizedProgress = Math.min(Math.max(progress, 0), 100);
    const progressLabel = normalizedProgress.toFixed(1);
    const elapsedLabel = this.formatDuration(elapsedSeconds);
    const remainingLabel = this.formatDuration(remainingSeconds);
    const progressMeta = this.formatProgressMeta(elapsedLabel, remainingLabel);
    void filesDownloaded;
    void filesTotal;
    void bytesDownloaded;
    void throughputBps;
    return (
      <div className={styles.card}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.title)}</h1>
          <p>{currentStep || intl.formatMessage(messages.progressLabel)}</p>
        </div>
        <div className={styles.progress}>
          <ProgressBarLarge
            progress={normalizedProgress}
            leftLabel={intl.formatMessage(messages.progressLabel)}
            rightLabel1={`${progressLabel}%`}
            isDarkMode
          />
          {progressMeta && (
            <div className={styles.progressMeta}>{progressMeta}</div>
          )}
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.cancel)}
            onClick={onCancel}
          />
        </div>
      </div>
    );
  }

  formatDuration(value?: number) {
    if (value == null || Number.isNaN(value)) return null;
    const totalSeconds = Math.max(0, Math.floor(value));
    const hours = Math.floor(totalSeconds / 3600);
    const minutes = Math.floor((totalSeconds % 3600) / 60);
    const seconds = totalSeconds % 60;
    if (hours > 0) {
      return `${hours}:${String(minutes).padStart(2, '0')}:${String(
        seconds
      ).padStart(2, '0')}`;
    }
    return `${minutes}:${String(seconds).padStart(2, '0')}`;
  }

  formatProgressMeta(
    elapsedLabel: string | null,
    remainingLabel: string | null
  ) {
    const { intl } = this.context;
    if (elapsedLabel && remainingLabel) {
      return intl.formatMessage(messages.progressTiming, {
        elapsed: elapsedLabel,
        remaining: remainingLabel,
      });
    }
    if (elapsedLabel) {
      return intl.formatMessage(messages.progressElapsed, {
        elapsed: elapsedLabel,
      });
    }
    if (remainingLabel) {
      return intl.formatMessage(messages.progressRemaining, {
        remaining: remainingLabel,
      });
    }
    return null;
  }

  renderError() {
    const { intl } = this.context;
    const { error, onWipeRetry, onDecline } = this.props;
    return (
      <div className={styles.card}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.errorTitle)}</h1>
          <p>{error?.message}</p>
          <div className={styles.errorHint}>
            {intl.formatMessage(messages.startFailureHint)}
          </div>
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.wipeAndRetry)}
            onClick={onWipeRetry}
          />
          <Button
            className={styles.secondaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.decline)}
            onClick={onDecline}
          />
        </div>
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
