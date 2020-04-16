// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactModal from 'react-modal';
import { observer } from 'mobx-react';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import styles from './WalletSelectImportDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import { WalletImportStatuses } from '../../../types/walletExportTypes';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import InlineEditingSmallInput from '../../widgets/forms/InlineEditingSmallInput';
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import infoIcon from '../../../assets/images/info-icon.inline.svg';
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
      '!!!These wallets were found in your Daedalus state directory.<pPlease select the wallets you want to import.</p>',
    description:
      'These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
  },
  unamedWalletsTitle: {
    id: 'wallet.select.import.dialog.unamedWalletsTitle',
    defaultMessage: '!!!Unnamed wallets',
    description: 'unamedWalletsTitle',
  },
  unamedWalletsTooltip: {
    id: 'wallet.select.import.dialog.unamedWalletsTooltip',
    defaultMessage:
      '!!!The following wallets were found in your secret keys file, but matching entries were not found in your wallet database. They are probably wallets you have previously deleted, or are not in your wallet database because of data corruption or a similar issue. You can ignore them, or you can try importing them if wallets are missing from the list of found walle',
    description: 'unamedWalletsTooltip',
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
  enterWalletNameTooltip: {
    id: 'wallet.select.import.dialog.enterWalletNameTooltip',
    defaultMessage: '!!!Enter a wallet name first',
    description: 'Enter a wallet name first',
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
  closeWindow: {
    id: 'wallet.select.import.dialog.closeWindow',
    defaultMessage: '!!!Close window',
    description: 'Close window',
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
    const walletImportedStatus = intl.formatMessage(messages.walletImported);

    let walletStatus;
    if (wallet.import.status === WalletImportStatuses.RUNNING) {
      walletStatus = importingStatus;
    } else if (wallet.import.status === WalletImportStatuses.EXISTS) {
      walletStatus = alreadyExistsStatus;
    } else if (wallet.import.status === WalletImportStatuses.COMPLETED) {
      walletStatus = walletImportedStatus;
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
      const disabled = wallet.name === null || !nameValidator(wallet.name);
      statusIcon = (
        <Checkbox
          onChange={() => {
            onToggleWalletImportSelection(wallet.id);
          }}
          checked={wallet.import.status === WalletImportStatuses.PENDING}
          disabled={disabled}
          skin={CheckboxSkin}
        />
      );
      if (disabled) {
        statusIcon = (
          <Tooltip
            className={styles.enterWalletNameTooltip}
            skin={TooltipSkin}
            tip={this.context.intl.formatMessage(
              messages.enterWalletNameTooltip
            )}
            arrowRelativeToTip
          >
            {statusIcon}
          </Tooltip>
        );
      }
    } else if (wallet.import.status === WalletImportStatuses.RUNNING) {
      statusIcon = <LoadingSpinner medium />;
    } else if (
      wallet.import.status === WalletImportStatuses.COMPLETED ||
      wallet.import.status === WalletImportStatuses.EXISTS
    ) {
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
            <div className={styles.topWrapper}>
              <div className={styles.title}>{title}</div>
              <div className={styles.description}>
                <FormattedHTMLMessage {...messages.description} />
              </div>
              <hr className={styles.separatorTop} />
            </div>
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
                          wallet.import.status ===
                            WalletImportStatuses.EXISTS ||
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

              {!!walletsWithoutNames.length && (
                <div className={styles.unamedWalletsTitle}>
                  {!!walletsWithNames.length && (
                    <hr className={styles.separatorMiddle} />
                  )}
                  <p>{intl.formatMessage(messages.unamedWalletsTitle)}</p>
                  <Tooltip
                    className={styles.unamedWalletsTooltip}
                    skin={TooltipSkin}
                    tip={intl.formatMessage(messages.unamedWalletsTooltip)}
                    arrowRelativeToTip
                  >
                    <SVGInline
                      svg={infoIcon}
                      className={styles.walletsStatusIconCheckmark}
                    />
                  </Tooltip>
                </div>
              )}

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
                          wallet.import.status ===
                            WalletImportStatuses.EXISTS ||
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
              <Link
                className={styles.closeWindowLink}
                onClick={onClose}
                label={intl.formatMessage(messages.closeWindow)}
                skin={LinkSkin}
                hasIconAfter={false}
              />
            </div>
          </div>
        </div>
      </ReactModal>
    );
  }
}
