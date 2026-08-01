import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import MithrilSyncOverlay from '../../components/loading/mithril-bootstrap/MithrilSyncOverlay';

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
    const { mithrilSync } = this.props.stores;
    mithrilSync.loadSnapshots();
    mithrilSync.syncStatus();
  }

  componentDidUpdate(prevProps: Props, prevState: { selectedDigest: string }) {
    if (
      prevProps.stores.mithrilSync.snapshots !==
        this.props.stores.mithrilSync.snapshots ||
      prevState.selectedDigest !== this.state.selectedDigest
    ) {
      this.ensureValidSelection();
    }
  }

  ensureValidSelection = () => {
    const { snapshots } = this.props.stores.mithrilSync;
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
    const { mithrilSync } = this.props.stores;
    const selected = this.state.selectedDigest;
    try {
      await mithrilSync.setDecision('accept');
      await mithrilSync.startBootstrap(
        selected && selected !== 'latest' ? selected : undefined,
        { wipeChain: false }
      );
    } catch (error) {
      // Errors are surfaced via Mithril status updates; avoid unhandled rejections.
    }
  };

  handleDecline = async () => {
    await this.props.stores.mithrilSync.setDecision('decline');
  };

  handleWipeRetry = async () => {
    const selected = this.state.selectedDigest;
    try {
      await this.props.stores.mithrilSync.startBootstrap(
        selected && selected !== 'latest' ? selected : undefined,
        { wipeChain: true }
      );
    } catch (error) {
      // Errors are surfaced via Mithril status updates; avoid unhandled rejections.
    }
  };

  handleCancel = async () => {
    await this.props.stores.mithrilSync.cancelBootstrap();
  };

  handleReturnToStorageLocation = async () => {
    await this.props.stores.mithrilSync.returnToStorageLocation();
  };

  handleOpenExternalLink = (url: string) => {
    this.props.stores.app.openExternalLink(url);
  };

  getLatestSnapshot = () => {
    const { snapshots } = this.props.stores.mithrilSync;
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
    const { mithrilSync } = this.props.stores;
    const latestSnapshot = this.getLatestSnapshot();
    const selectedSnapshot =
      this.state.selectedDigest === 'latest'
        ? latestSnapshot
        : mithrilSync.snapshots.find(
            (snapshot) => snapshot.digest === this.state.selectedDigest
          );

    return (
      <MithrilSyncOverlay
        status={mithrilSync.status}
        flowType="bootstrap"
        customChainPath={mithrilSync.customChainPath}
        defaultChainPath={mithrilSync.defaultChainPath}
        defaultChainStorageValidation={
          mithrilSync.defaultChainStorageValidation
        }
        chainStorageValidation={mithrilSync.chainStorageValidation}
        pendingChainPath={mithrilSync.pendingChainPath}
        isRecoveryFallback={mithrilSync.isRecoveryFallback}
        latestSnapshotSize={latestSnapshot?.size}
        isChainStorageLoading={mithrilSync.isChainStorageLoading}
        isApplyingStorageLocation={mithrilSync.isApplyingStorageLocation}
        storageLocationConfirmed={mithrilSync.storageLocationConfirmed}
        snapshots={mithrilSync.snapshots}
        selectedDigest={this.state.selectedDigest}
        selectedSnapshot={selectedSnapshot || null}
        error={mithrilSync.error}
        isFetchingSnapshots={mithrilSync.isFetchingSnapshots}
        progressItems={mithrilSync.progressItems}
        filesDownloaded={mithrilSync.filesDownloaded}
        filesTotal={mithrilSync.filesTotal}
        snapshotBytesDownloaded={mithrilSync.snapshotBytesDownloaded}
        snapshotBytesTotal={mithrilSync.snapshotBytesTotal}
        ancillaryBytesDownloaded={mithrilSync.ancillaryBytesDownloaded}
        ancillaryBytesTotal={mithrilSync.ancillaryBytesTotal}
        ancillaryProgress={mithrilSync.ancillaryProgress}
        bootstrapStartedAt={mithrilSync.bootstrapStartedAt}
        canRetry={false}
        canRestartNormally={false}
        canWipeAndFullSync={false}
        onOpenExternalLink={this.handleOpenExternalLink}
        onSetChainStorageDirectory={mithrilSync.setChainStorageDirectory}
        onResetChainStorageDirectory={mithrilSync.resetChainStorageDirectory}
        onValidateChainStorageDirectory={
          mithrilSync.validateChainStorageDirectory
        }
        onConfirmStorageLocation={mithrilSync.confirmStorageLocation}
        onReturnToStorageLocation={this.handleReturnToStorageLocation}
        onSelectSnapshot={this.handleSelectSnapshot}
        onAccept={this.handleAccept}
        onDecline={this.handleDecline}
        onWipeRetry={this.handleWipeRetry}
        onCancel={this.handleCancel}
        onRetry={() => {}}
        onRestartNormally={() => {}}
        onWipeAndFullSync={() => {}}
        onDismissCompleted={() => {}}
        onQuit={() => this.props.actions.window.closeWindow.trigger()}
      />
    );
  }
}

export default MithrilBootstrapPage;
