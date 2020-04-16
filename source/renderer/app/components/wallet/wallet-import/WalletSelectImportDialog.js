// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import ReactModal from 'react-modal';
import { observer } from 'mobx-react';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import classNames from 'classnames';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import SVGInline from 'react-svg-inline';
import styles from './WalletSelectImportDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import { WalletImportStatuses } from '../../../types/walletExportTypes';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import InlineEditingSmallInput from '../../widgets/forms/InlineEditingSmallInput';
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import type { ExportedByronWallet } from '../../../types/walletExportTypes';

const messages = defineMessages({
  title: {
    id: 'wallet.select.import.dialog.title',
    defaultMessage: '!!!Found wallets',
    description: 'Select import wallets dialog title',
  },
  description: {
    id: 'wallet.select.import.dialog.description',
    defaultMessage:
      '!!!These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
    description:
      'These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
  },
  passwordProtected: {
    id: 'wallet.select.import.dialog.passwordProtected',
    defaultMessage: '!!!Password protected',
    description: 'Password protected',
  },
  walletExists: {
    id: 'wallet.select.import.dialog.walletExists',
    defaultMessage: '!!!Wallet already exists',
    description: 'Wallet already exists',
  },
  noPassword: {
    id: 'wallet.select.import.dialog.noPassword',
    defaultMessage: '!!!No password',
    description: 'No password',
  },
  importingWallet: {
    id: 'wallet.select.import.dialog.importingWallet',
    defaultMessage: '!!!Importing wallet...',
    description: 'Importing wallet...',
  },
  walletName: {
    id: 'wallet.select.import.dialog.walletName',
    defaultMessage: '!!!Enter wallet name',
    description: 'Enter wallet name',
  },
  notFound: {
    id: 'wallet.select.import.dialog.notFound',
    defaultMessage: '!!!Name not found',
    description: 'Name not found',
  },
  walletImported: {
    id: 'wallet.select.import.dialog.walletImported',
    defaultMessage: '!!!Wallet imported',
    description: 'Wallet imported',
  },
  buttonLabel: {
    id: 'wallet.select.import.dialog.buttonLabel',
    defaultMessage: '!!!Import selected wallets',
    description: 'Import selected wallets',
  },
});

type Props = {
  isSubmitting: boolean,
  exportedWallets: Array<ExportedByronWallet>,
  pendingImportWalletsCount: number,
  onConfirm: Function,
  onWalletNameChange: Function,
  onToggleWalletImportSelection: Function,
  onClose: Function,
  nameValidator: Function,
};

@observer
export default class WalletSelectImportDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  getWalletStatus = (wallet: ExportedByronWallet) => {
    const { intl } = this.context;
    const importingStatus = intl.formatMessage(messages.importingWallet);
    const noPasswordStatus = intl.formatMessage(messages.noPassword);
    const hasPasswordStatus = intl.formatMessage(messages.passwordProtected);
    const alreadyExistsStatus = intl.formatMessage(messages.walletExists);
    // const walletImportedStatus = intl.formatMessage(messages.walletImported);
    // const walletNotFoundStatus = intl.formatMessage(messages.notFound);

    let walletStatus;
    if (wallet.import.status === WalletImportStatuses.RUNNING) {
      walletStatus = importingStatus;
    } else if (wallet.import.status === WalletImportStatuses.COMPLETED) {
      walletStatus = alreadyExistsStatus;
    } else if (wallet.is_passphrase_empty) {
      walletStatus = noPasswordStatus;
    } else {
      walletStatus = hasPasswordStatus;
    }
    return walletStatus;
  };

  getWalletStatusIcon = (wallet: ExportedByronWallet) => {
    const { nameValidator, onToggleWalletImportSelection } = this.props;
    let statusIcon;
    if (
      wallet.import.status === WalletImportStatuses.UNSTARTED ||
      wallet.import.status === WalletImportStatuses.PENDING ||
      wallet.import.status === WalletImportStatuses.ERRORED
    ) {
      statusIcon = (
        <Checkbox
          onChange={() => {
            onToggleWalletImportSelection(wallet.id);
          }}
          checked={wallet.import.status === WalletImportStatuses.PENDING}
          disabled={wallet.name === null || !nameValidator(wallet.name)}
          skin={CheckboxSkin}
        />
      );
    } else if (wallet.import.status === WalletImportStatuses.RUNNING) {
      statusIcon = <LoadingSpinner medium />;
    } else if (wallet.import.status === WalletImportStatuses.COMPLETED) {
      statusIcon = (
        <SVGInline
          svg={checkmarkImage}
          className={styles.walletsStatusIconCheckmark}
        />
      );
    }
    return statusIcon;
  };

  render() {
    const { intl } = this.context;
    const {
      isSubmitting,
      exportedWallets,
      pendingImportWalletsCount,
      onConfirm,
      onClose,
      onWalletNameChange,
      nameValidator,
    } = this.props;

    const title = intl.formatMessage(messages.title);
    const description = intl.formatMessage(messages.description);
    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.buttonLabel)
    ) : (
      <LoadingSpinner />
    );

    const isDisabled = isSubmitting || !pendingImportWalletsCount;
    const buttonClasses = classNames(styles.actionButton, [
      isDisabled ? styles.disabled : null,
    ]);

    const walletsWithNames = exportedWallets.filter(
      ({ hasName }: ExportedByronWallet) => hasName
    );
    const walletsWithoutNames = exportedWallets.filter(
      ({ hasName }: ExportedByronWallet) => !hasName
    );
    let walletIndex = 0;

    return (
      <ReactModal
        isOpen
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        shouldCloseOnEsc={false}
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
            <div className={styles.title}>{title}</div>
            <div className={styles.description}>{description}</div>
            <hr className={styles.separator} />
            <div className={styles.walletsContainer}>
              {walletsWithNames.map(wallet => {
                walletIndex++;
                return (
                  <div className={styles.walletsRow} key={wallet.id}>
                    <div
                      className={styles.walletsCounter}
                    >{`${walletIndex}.`}</div>
                    <div className={styles.walletsInputField}>
                      <InlineEditingSmallInput
                        isActive={false}
                        isDisabled={
                          wallet.import.status ===
                            WalletImportStatuses.COMPLETED ||
                          wallet.import.status === WalletImportStatuses.RUNNING
                        }
                        inputFieldValue={wallet.name || ''}
                        placeholder={intl.formatMessage(messages.walletName)}
                        isValid={nameValidator}
                        validationErrorMessage={intl.formatMessage(
                          globalMessages.invalidWalletName
                        )}
                        onSubmit={(name: string) =>
                          onWalletNameChange({
                            id: wallet.id,
                            name,
                          })
                        }
                        maxLength={40}
                        successfullyUpdated
                      />
                    </div>
                    <div className={styles.walletsStatus}>
                      {this.getWalletStatus(wallet)}
                    </div>
                    <div className={styles.walletsStatusIcon}>
                      {this.getWalletStatusIcon(wallet)}
                    </div>
                  </div>
                );
              })}
              {walletsWithNames.length && walletsWithoutNames.length ? (
                <hr className={styles.separator} />
              ) : null}
              {walletsWithoutNames.map(wallet => {
                walletIndex++;
                return (
                  <div className={styles.walletsRow} key={wallet.id}>
                    <div
                      className={styles.walletsCounter}
                    >{`${walletIndex}.`}</div>
                    <div className={styles.walletsInputField}>
                      <InlineEditingSmallInput
                        isActive={false}
                        isDisabled={
                          wallet.import.status ===
                            WalletImportStatuses.COMPLETED ||
                          wallet.import.status === WalletImportStatuses.RUNNING
                        }
                        inputFieldValue={wallet.name || ''}
                        placeholder={intl.formatMessage(messages.notFound)}
                        isValid={nameValidator}
                        validationErrorMessage={intl.formatMessage(
                          globalMessages.invalidWalletName
                        )}
                        onSubmit={(name: string) =>
                          onWalletNameChange({
                            id: wallet.id,
                            name,
                          })
                        }
                        successfullyUpdated
                      />
                    </div>
                    <div className={styles.walletsStatus}>
                      {this.getWalletStatus(wallet)}
                    </div>
                    <div className={styles.walletsStatusIcon}>
                      {this.getWalletStatusIcon(wallet)}
                    </div>
                  </div>
                );
              })}
            </div>
            <div className={styles.action}>
              <Button
                className={buttonClasses}
                disabled={isDisabled}
                label={buttonLabel}
                onClick={onConfirm}
                skin={ButtonSkin}
              />
            </div>
          </div>
        </div>
      </ReactModal>
    );
  }
}
