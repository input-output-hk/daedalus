// @flow
import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import ReactModal from 'react-modal';
import styles from './WalletSelectImportDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';

type Props = {
  onConfirm: Function,
  onClose: Function,
};

export default class WalletSelectImportDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  confirm = () => {
    this.props.onConfirm();
  };

  render() {
    const {
      onConfirm,
      onClose,
    } = this.props;

    return (
      <ReactModal
        isOpen
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        className={styles.dialog}
        overlayClassName={styles.overlay}
        ariaHideApp={false}
      >
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={onClose}
          />
          <div className={styles.backgroundContainer} />
          <div className={styles.content}>
            Dialog 2
          </div>
        </div>
      </ReactModal>
    );
  }
}
