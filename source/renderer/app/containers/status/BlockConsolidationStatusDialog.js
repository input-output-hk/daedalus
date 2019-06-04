// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import styles from './BlockConsolidationStatusDialog.scss';

@inject('stores', 'actions')
@observer
export default class BlockConsolidationStatusDialog extends Component<InjectedProps> {
  componentWillMount() {
    this.props.actions.blockConsolidation.startBlockConsolidationDataPolling.trigger();
  }

  componeneWillUnmount() {
    this.props.actions.blockConsolidation.stopBlockConsolidationDataPolling.trigger();
  }

  handleClose = () => {
    this.props.actions.app.toggleBlockConsolidationStatusScreen.trigger();
  };

  render() {
    const { app, blockConsolidation, daedalusDiagnostics } = this.props.stores;
    const { openExternalLink } = app;
    const { epochsConsolidated, currentEpoch } = blockConsolidation;
    const { syncProgress } = daedalusDiagnostics;
    return (
      <ReactModal
        isOpen
        onRequestClose={this.handleClose}
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <BlockConsolidationStatus
          currentEpoch={currentEpoch}
          epochsConsolidated={epochsConsolidated}
          epochsSynced={syncProgress}
          onExternalLinkClick={openExternalLink}
          onClose={this.handleClose}
        />
      </ReactModal>
    );
  }
}
