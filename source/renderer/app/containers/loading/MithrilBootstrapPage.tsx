import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import MithrilBootstrap from '../../components/loading/mithril-bootstrap/MithrilBootstrap';

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

  getLatestSnapshot = () => {
    const { snapshots } = this.props.stores.mithrilBootstrap;
    if (!snapshots.length) return null;
    return snapshots.reduce((latest, snapshot) => {
      if (!latest) return snapshot;
      const latestTime = Date.parse(latest.createdAt);
      const nextTime = Date.parse(snapshot.createdAt);
      if (Number.isNaN(nextTime)) return latest;
      if (Number.isNaN(latestTime) || nextTime > latestTime) return snapshot;
      return latest;
    }, null as any);
  };

  render() {
    const { mithrilBootstrap } = this.props.stores;
    const selectedSnapshot =
      this.state.selectedDigest === 'latest'
        ? this.getLatestSnapshot()
        : mithrilBootstrap.snapshots.find(
            (snapshot) => snapshot.digest === this.state.selectedDigest
          );
    return (
      <MithrilBootstrap
        status={mithrilBootstrap.status}
        progress={mithrilBootstrap.progress}
        currentStep={mithrilBootstrap.currentStep}
        filesDownloaded={mithrilBootstrap.filesDownloaded}
        filesTotal={mithrilBootstrap.filesTotal}
        bytesDownloaded={mithrilBootstrap.bytesDownloaded}
        throughputBps={mithrilBootstrap.throughputBps}
        elapsedSeconds={mithrilBootstrap.elapsedSeconds}
        remainingSeconds={mithrilBootstrap.remainingSeconds}
        snapshots={mithrilBootstrap.snapshots}
        selectedDigest={this.state.selectedDigest}
        selectedSnapshot={selectedSnapshot || null}
        error={mithrilBootstrap.error}
        isFetchingSnapshots={mithrilBootstrap.isFetchingSnapshots}
        onSelectSnapshot={this.handleSelectSnapshot}
        onAccept={this.handleAccept}
        onDecline={this.handleDecline}
        onWipeRetry={this.handleWipeRetry}
        onCancel={this.handleCancel}
      />
    );
  }
}

export default MithrilBootstrapPage;
