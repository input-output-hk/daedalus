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

  handleSelectSnapshot = (digest: string | null) => {
    this.setState({
      selectedDigest: digest || 'latest',
    });
  };

  handleAccept = async () => {
    const { mithrilBootstrap } = this.props.stores;
    const selected = this.state.selectedDigest;
    await mithrilBootstrap.setDecision('accept');
    await mithrilBootstrap.startBootstrap(
      selected && selected !== 'latest' ? selected : undefined
    );
  };

  handleDecline = async () => {
    await this.props.stores.mithrilBootstrap.setDecision('decline');
  };

  handleRetry = async () => {
    const selected = this.state.selectedDigest;
    await this.props.stores.mithrilBootstrap.startBootstrap(
      selected && selected !== 'latest' ? selected : undefined
    );
  };

  handleCancel = async () => {
    await this.props.stores.mithrilBootstrap.cancelBootstrap();
  };

  render() {
    const { mithrilBootstrap } = this.props.stores;
    return (
      <MithrilBootstrap
        status={mithrilBootstrap.status}
        progress={mithrilBootstrap.progress}
        currentStep={mithrilBootstrap.currentStep}
        snapshots={mithrilBootstrap.snapshots}
        selectedDigest={this.state.selectedDigest}
        error={mithrilBootstrap.error}
        isFetchingSnapshots={mithrilBootstrap.isFetchingSnapshots}
        onSelectSnapshot={this.handleSelectSnapshot}
        onAccept={this.handleAccept}
        onDecline={this.handleDecline}
        onRetry={this.handleRetry}
        onCancel={this.handleCancel}
      />
    );
  }
}

export default MithrilBootstrapPage;
