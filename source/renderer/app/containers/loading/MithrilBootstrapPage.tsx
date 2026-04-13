import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import { MithrilBootstrap } from '../../components/loading/mithril-bootstrap';

const PROGRESS_VIEW_STATUSES = [
  'preparing',
  'downloading',
  'verifying',
  'unpacking',
  'finalizing',
  'converting',
  'completed',
  'starting-node',
] as const;

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class MithrilBootstrapPage extends Component<Props> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  state = {
    selectedDigest: 'latest',
  };

  private _cachedLatestSnapshot: any = null;
  private _cachedSnapshotsRef: any = null;

  componentDidMount() {
    const { mithrilBootstrap } = this.props.stores;
    mithrilBootstrap.loadSnapshots();
    mithrilBootstrap.syncStatus();
  }

  componentDidUpdate(prevProps: Props, prevState: { selectedDigest: string }) {
    if (
      prevProps.stores.mithrilBootstrap.snapshots !==
        this.props.stores.mithrilBootstrap.snapshots ||
      prevState.selectedDigest !== this.state.selectedDigest
    ) {
      this.ensureValidSelection();
    }
  }

  ensureValidSelection = () => {
    const { snapshots } = this.props.stores.mithrilBootstrap;
    const selectedDigest = this.state.selectedDigest || 'latest';
    if (selectedDigest === 'latest') return;
    const exists = snapshots.some(
      (snapshot) => snapshot.digest === selectedDigest
    );
    if (!exists) {
      this.setState({ selectedDigest: 'latest' });
    }
  };

  handleSelectSnapshot = (digest: string | null) => {
    this.setState({
      selectedDigest: digest || 'latest',
    });
  };

  handleAccept = async () => {
    const { mithrilBootstrap } = this.props.stores;
    const selected = this.state.selectedDigest;
    try {
      await mithrilBootstrap.setDecision('accept');
      await mithrilBootstrap.startBootstrap(
        selected && selected !== 'latest' ? selected : undefined,
        { wipeChain: false }
      );
    } catch (error) {
      // Errors are surfaced via Mithril status updates; avoid unhandled rejections.
    }
  };

  handleDecline = async () => {
    await this.props.stores.mithrilBootstrap.setDecision('decline');
  };

  handleWipeRetry = async () => {
    const selected = this.state.selectedDigest;
    try {
      await this.props.stores.mithrilBootstrap.startBootstrap(
        selected && selected !== 'latest' ? selected : undefined,
        { wipeChain: true }
      );
    } catch (error) {
      // Errors are surfaced via Mithril status updates; avoid unhandled rejections.
    }
  };

  handleCancel = async () => {
    await this.props.stores.mithrilBootstrap.cancelBootstrap();
  };

  handleReturnToStorageLocation = () => {
    this.props.stores.mithrilBootstrap.clearStorageLocationConfirmation();
  };

  handleOpenExternalLink = (url: string) => {
    this.props.stores.app.openExternalLink(url);
  };

  getLatestSnapshot = () => {
    const { snapshots } = this.props.stores.mithrilBootstrap;
    if (snapshots === this._cachedSnapshotsRef)
      return this._cachedLatestSnapshot;
    this._cachedSnapshotsRef = snapshots;
    if (!snapshots.length) {
      this._cachedLatestSnapshot = null;
      return null;
    }
    this._cachedLatestSnapshot = snapshots.reduce((latest, snapshot) => {
      if (!latest) return snapshot;
      const latestTime = new Date(latest.createdAt).getTime();
      const nextTime = new Date(snapshot.createdAt).getTime();
      if (Number.isNaN(nextTime)) return latest;
      if (Number.isNaN(latestTime) || nextTime > latestTime) return snapshot;
      return latest;
    }, null as any);
    return this._cachedLatestSnapshot;
  };

  render() {
    const { mithrilBootstrap } = this.props.stores;
    const latestSnapshot = this.getLatestSnapshot();
    const isProgressVisible = PROGRESS_VIEW_STATUSES.includes(
      mithrilBootstrap.status
    );
    const selectedSnapshot =
      this.state.selectedDigest === 'latest'
        ? latestSnapshot
        : mithrilBootstrap.snapshots.find(
            (snapshot) => snapshot.digest === this.state.selectedDigest
          );
    const progressProps = isProgressVisible
      ? {
          bytesDownloaded: mithrilBootstrap.bytesDownloaded,
          snapshotSize: mithrilBootstrap.snapshot?.size,
          ancillaryBytesDownloaded: mithrilBootstrap.ancillaryBytesDownloaded,
          ancillaryBytesTotal: mithrilBootstrap.ancillaryBytesTotal,
          ancillaryProgress: mithrilBootstrap.ancillaryProgress,
          progressItems: mithrilBootstrap.progressItems,
          bootstrapStartedAt: mithrilBootstrap.bootstrapStartedAt,
        }
      : {};

    return (
      <MithrilBootstrap
        status={mithrilBootstrap.status}
        customChainPath={mithrilBootstrap.customChainPath}
        defaultChainPath={mithrilBootstrap.defaultChainPath}
        defaultChainStorageValidation={
          mithrilBootstrap.defaultChainStorageValidation
        }
        chainStorageValidation={mithrilBootstrap.chainStorageValidation}
        latestSnapshotSize={latestSnapshot?.size}
        isChainStorageLoading={mithrilBootstrap.isChainStorageLoading}
        storageLocationConfirmed={mithrilBootstrap.storageLocationConfirmed}
        snapshots={mithrilBootstrap.snapshots}
        selectedDigest={this.state.selectedDigest}
        selectedSnapshot={selectedSnapshot || null}
        error={mithrilBootstrap.error}
        isFetchingSnapshots={mithrilBootstrap.isFetchingSnapshots}
        onOpenExternalLink={this.handleOpenExternalLink}
        onSetChainStorageDirectory={mithrilBootstrap.setChainStorageDirectory}
        onResetChainStorageDirectory={
          mithrilBootstrap.resetChainStorageDirectory
        }
        onValidateChainStorageDirectory={
          mithrilBootstrap.validateChainStorageDirectory
        }
        onConfirmStorageLocation={mithrilBootstrap.confirmStorageLocation}
        onReturnToStorageLocation={this.handleReturnToStorageLocation}
        onSelectSnapshot={this.handleSelectSnapshot}
        onAccept={this.handleAccept}
        onDecline={this.handleDecline}
        onWipeRetry={this.handleWipeRetry}
        onCancel={this.handleCancel}
        {...progressProps}
      />
    );
  }
}

export default MithrilBootstrapPage;
