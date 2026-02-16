import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import ProgressBarLarge from '../../widgets/ProgressBarLarge';
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
  progressLabel: {
    id: 'loading.mithrilBootstrap.progressLabel',
    defaultMessage: '!!!Bootstrap progress',
    description: 'Label for Mithril progress bar.',
  },
  cancel: {
    id: 'loading.mithrilBootstrap.cancel',
    defaultMessage: '!!!Cancel',
    description: 'Button label to cancel Mithril bootstrap.',
  },
  retry: {
    id: 'loading.mithrilBootstrap.retry',
    defaultMessage: '!!!Try again',
    description: 'Button label to retry Mithril bootstrap.',
  },
  errorTitle: {
    id: 'loading.mithrilBootstrap.errorTitle',
    defaultMessage: '!!!Mithril bootstrap failed',
    description: 'Title for Mithril error state.',
  },
});

type Props = {
  status: MithrilBootstrapStatus;
  progress: number;
  currentStep?: string;
  snapshots: Array<MithrilSnapshotItem>;
  selectedDigest?: string | null;
  error?: MithrilBootstrapError | null;
  isFetchingSnapshots: boolean;
  onSelectSnapshot: (digest: string | null) => void;
  onAccept: () => void;
  onDecline: () => void;
  onRetry: () => void;
  onCancel: () => void;
};

type SnapshotOption = {
  value: string;
  label: string;
};

@observer
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
          <div className={styles.selector}>
            <Select
              skin={SelectSkin}
              className={styles.selectInput}
              options={options}
              value={selectedValue}
              disabled={isFetchingSnapshots}
              onChange={(option) =>
                onSelectSnapshot(option ? option.value : null)
              }
            />
          </div>
        </div>
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
    const { progress, currentStep, onCancel } = this.props;
    return (
      <div className={styles.card}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.title)}</h1>
          <p>{currentStep || intl.formatMessage(messages.progressLabel)}</p>
        </div>
        <div className={styles.progress}>
          <ProgressBarLarge
            progress={Math.round(progress)}
            leftLabel={intl.formatMessage(messages.progressLabel)}
            rightLabel1={`${Math.round(progress)}%`}
            isDarkMode
          />
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

  renderError() {
    const { intl } = this.context;
    const { error, onRetry, onDecline } = this.props;
    return (
      <div className={styles.card}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.errorTitle)}</h1>
          <p>{error?.message}</p>
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.retry)}
            onClick={onRetry}
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

export default MithrilBootstrap;
