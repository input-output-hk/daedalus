// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ReactModal from 'react-modal';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import styles from './BlockConsolidationStatusDialog.scss';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class BlockConsolidationStatusDialog extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  componentWillMount() {
    this.props.actions.blockConsolidation.startBlockConsolidationDataPolling.trigger();
  }

  componeneWillUnmount() {
    this.props.actions.blockConsolidation.stopBlockConsolidationDataPolling.trigger();
  }

  handleClose = () => {
    this.props.actions.app.closeBlockConsolidationStatusDialog.trigger();
  };

  render() {
    const { app, blockConsolidation, networkStatus } = this.props.stores;
    const { openExternalLink } = app;
    const { epochsConsolidated, currentEpoch } = blockConsolidation;
    const { syncProgress } = networkStatus;
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
