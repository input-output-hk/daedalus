import React, { Component } from 'react';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import { observer } from 'mobx-react';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSelectImportDialog.scs... Remove this comment to see the full error message
import styles from './WalletSelectImportDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeCrossThin from '../../../assets/images/close-cross-thin.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import { WalletImportStatuses } from '../../../types/walletExportTypes';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import InlineEditingSmallInput from '../../widgets/forms/InlineEditingSmallInput';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/check-w... Remove this comment to see the full error message
import checkmarkImage from '../../../assets/images/check-w.inline.svg';
import { MAX_ADA_WALLETS_COUNT } from '../../../config/numbersConfig';
import type { ExportedByronWallet } from '../../../types/walletExportTypes';
import Dialog from '../../widgets/Dialog';

const messages = defineMessages({
  title: {
    id: 'wallet.select.import.dialog.title',
    defaultMessage: '!!!Found wallets',
    description: 'Select import wallets dialog title',
  },
  description: {
    id: 'wallet.select.import.dialog.description',
    defaultMessage:
      '!!!These wallets were found in your Daedalus state directory.<p>Please select the wallets you want to import.</p>',
    description:
      'These wallets were found in your Daedalus state directory. Please select the wallets you want to import.',
  },
  unamedWalletsTitle: {
    id: 'wallet.select.import.dialog.unamedWalletsTitle',
    defaultMessage: '!!!Unnamed wallets',
    description: 'unamedWalletsTitle',
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
  maxWalletsReachedTooltip: {
    id: 'wallet.select.import.dialog.maxWalletsReachedTooltip',
    defaultMessage:
      '!!!Daedalus supports up to {maxWalletsCount} wallets. You will need to remove another wallet before you can import this one.',
    description: 'Max number of wallets reached',
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
  linkLabel: {
    id: 'wallet.select.import.dialog.linkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more',
  },
  linkUrl: {
    id: 'wallet.select.import.dialog.linkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/900000623463',
    description: '"Learn more" link URL on the wallet import file dialog',
  },
  closeWindow: {
    id: 'wallet.select.import.dialog.closeWindow',
    defaultMessage: '!!!Close window',
    description: 'Close window',
  },
});
type Props = {
  isSubmitting: boolean;
  exportedWallets: Array<ExportedByronWallet>;
  pendingImportWalletsCount: number;
  onContinue: (...args: Array<any>) => any;
  onWalletNameChange: (...args: Array<any>) => any;
  onToggleWalletImportSelection: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onOpenExternalLink: (...args: Array<any>) => any;
  nameValidator: (...args: Array<any>) => any;
  isMaxNumberOfWalletsReached: boolean;
};

@observer
class WalletSelectImportDialog extends Component<Props> {
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
    } else if (wallet.isEmptyPassphrase) {
      walletStatus = noPasswordStatus;
    } else {
      walletStatus = hasPasswordStatus;
    }

    return walletStatus;
  };
  getWalletStatusIcon = (wallet: ExportedByronWallet, index: number) => {
    const {
      nameValidator,
      onToggleWalletImportSelection,
      isMaxNumberOfWalletsReached,
    } = this.props;
    let statusIcon;

    if (
      wallet.import.status === WalletImportStatuses.UNSTARTED ||
      wallet.import.status === WalletImportStatuses.PENDING ||
      wallet.import.status === WalletImportStatuses.ERRORED
    ) {
      const invalidWalletName =
        wallet.name === null || !nameValidator(wallet.name);
      const walletNotSelectable =
        isMaxNumberOfWalletsReached &&
        wallet.import.status !== WalletImportStatuses.PENDING;
      const disabled = invalidWalletName || walletNotSelectable;
      let isOpeningUpward = true;
      const checkboxes = document.getElementsByClassName(
        'SimpleCheckbox_check'
      );

      if (checkboxes.length && wallet.hasName) {
        const topWrapper = document.getElementsByClassName(
          'WalletSelectImportDialog_topWrapper'
        );

        if (checkboxes[index] && topWrapper.length) {
          const checkboxTopOffset = checkboxes[index].getBoundingClientRect()
            .top;
          const topWrapperTopOffset = topWrapper[0].getBoundingClientRect().top;
          const topPart = topWrapperTopOffset + 121;
          const spaceForTooltip = checkboxTopOffset - topPart;

          if (
            (walletNotSelectable && spaceForTooltip < 85) ||
            (invalidWalletName && spaceForTooltip < 27)
          ) {
            isOpeningUpward = false;
          }
        }
      }

      statusIcon = (
        <Checkbox
          onChange={() => {
            onToggleWalletImportSelection({
              index: wallet.index,
            });
          }}
          checked={wallet.import.status === WalletImportStatuses.PENDING}
          disabled={disabled}
          skin={CheckboxSkin}
        />
      );

      if (disabled) {
        statusIcon = (
          <PopOver
            content={
              invalidWalletName ? (
                this.context.intl.formatMessage(messages.enterWalletNameTooltip)
              ) : (
                <FormattedMessage
                  {...messages.maxWalletsReachedTooltip}
                  values={{
                    maxWalletsCount: MAX_ADA_WALLETS_COUNT,
                  }}
                />
              )
            }
            isBounded={walletNotSelectable}
            placement={isOpeningUpward ? 'top' : 'bottom'}
          >
            {statusIcon}
          </PopOver>
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
  getInlineEditingSmallInput = (
    wallet: ExportedByronWallet,
    validationMessage: string,
    placeholderMessage: string,
    nameValidator: (...args: Array<any>) => any,
    onWalletNameChange: (...args: Array<any>) => any
  ) => {
    return (
      <InlineEditingSmallInput
        isActive={false}
        className={styles.walletsInputFieldInner}
        isDisabled={
          wallet.import.status === WalletImportStatuses.COMPLETED ||
          wallet.import.status === WalletImportStatuses.EXISTS ||
          wallet.import.status === WalletImportStatuses.RUNNING
        }
        inputFieldValue={wallet.name || ''}
        placeholder={placeholderMessage}
        isValid={nameValidator}
        validationErrorMessage={validationMessage}
        onSubmit={(name: string) =>
          onWalletNameChange({
            index: wallet.index,
            name,
          })
        }
        maxLength={40}
        successfullyUpdated
      />
    );
  };

  render() {
    const { intl } = this.context;
    const {
      isSubmitting,
      exportedWallets,
      pendingImportWalletsCount,
      onContinue,
      onClose,
      onOpenExternalLink,
      onWalletNameChange,
      nameValidator,
    } = this.props;
    const title = intl.formatMessage(messages.title);
    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.buttonLabel)
    ) : (
      <LoadingSpinner />
    );
    const linkLabel = intl.formatMessage(messages.linkLabel);

    const onLinkClick = () =>
      onOpenExternalLink(intl.formatMessage(messages.linkUrl));

    const walletsWithNames = exportedWallets.filter(
      ({ hasName }: ExportedByronWallet) => hasName
    );
    const walletsWithoutNames = exportedWallets.filter(
      ({ hasName }: ExportedByronWallet) => !hasName
    );
    let previousWalletId = '';
    let rowNumber = 1;
    const anyWalletWithoutName = walletsWithoutNames.filter(
      (item) =>
        (!item.name || item.name.length < 3) &&
        item.import.status === WalletImportStatuses.PENDING
    );
    const isDisabled =
      isSubmitting || anyWalletWithoutName.length || !pendingImportWalletsCount;
    const buttonClasses = classNames(styles.actionButton, [
      isDisabled ? styles.disabled : null,
    ]);
    return (
      <Dialog
        className={styles.dialog}
        closeOnOverlayClick={false}
        onClose={onClose}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onRequestClose={onClose}
        shouldCloseOnOverlayClick={false}
        shouldCloseOnEsc={false}
        defaultThemeOverrides
      >
        <div className={styles.component}>
          <DialogCloseButton
            className={styles.closeButton}
            icon={closeCrossThin}
            onClose={onClose}
          />
          <div className={styles.content}>
            <div className={styles.topWrapper}>
              <div className={styles.title}>{title}</div>
              <div className={styles.description}>
                <FormattedHTMLMessage {...messages.description} />
              </div>
              <hr className={styles.separatorTop} />
            </div>
            <div className={styles.walletsContainer}>
              {walletsWithNames.map((wallet, index) => {
                const isDuplicate = previousWalletId === wallet.id;
                const rowClasses = classNames([
                  styles.walletsRow,
                  'namedWalletsRow',
                ]);
                const walletRow = (
                  <div
                    className={rowClasses}
                    key={`${wallet.id}-${wallet.index}`}
                  >
                    <div className={styles.walletsCounter}>
                      {!isDuplicate && `${rowNumber}.`}
                    </div>
                    <div className={styles.walletsInputField}>
                      {this.getInlineEditingSmallInput(
                        wallet,
                        intl.formatMessage(globalMessages.invalidWalletName),
                        intl.formatMessage(messages.walletName),
                        nameValidator,
                        onWalletNameChange
                      )}
                    </div>
                    <div className={styles.walletsStatus}>
                      {this.getWalletStatus(wallet)}
                    </div>
                    <div className={styles.walletsStatusIcon}>
                      {this.getWalletStatusIcon(wallet, index)}
                    </div>
                  </div>
                );

                if (!isDuplicate) {
                  previousWalletId = wallet.id;
                  rowNumber++;
                }

                return walletRow;
              })}

              {!!walletsWithoutNames.length && (
                <div className={styles.unamedWalletsTitle}>
                  {!!walletsWithNames.length && (
                    <hr className={styles.separatorMiddle} />
                  )}
                  <p>{intl.formatMessage(messages.unamedWalletsTitle)}</p>
                </div>
              )}

              {walletsWithoutNames.map((wallet, index) => {
                const isDuplicate = previousWalletId === wallet.id;
                const rowClasses = classNames([
                  styles.walletsRow,
                  'unnamedWalletsRow',
                ]);
                const walletRow = (
                  <div
                    className={rowClasses}
                    key={`${wallet.id}-${wallet.index}`}
                  >
                    <div className={styles.walletsCounter}>
                      {!isDuplicate && `${rowNumber}.`}
                    </div>
                    <div className={styles.walletsInputField}>
                      {!wallet.name ? (
                        <PopOver
                          content={intl.formatMessage(
                            messages.enterWalletNameTooltip
                          )}
                        >
                          {this.getInlineEditingSmallInput(
                            wallet,
                            intl.formatMessage(
                              globalMessages.invalidWalletName
                            ),
                            intl.formatMessage(messages.notFound),
                            nameValidator,
                            onWalletNameChange
                          )}
                        </PopOver>
                      ) : (
                        <>
                          {this.getInlineEditingSmallInput(
                            wallet,
                            intl.formatMessage(
                              globalMessages.invalidWalletName
                            ),
                            intl.formatMessage(messages.notFound),
                            nameValidator,
                            onWalletNameChange
                          )}
                        </>
                      )}
                    </div>
                    <div className={styles.walletsStatus}>
                      {this.getWalletStatus(wallet)}
                    </div>
                    <div className={styles.walletsStatusIcon}>
                      {this.getWalletStatusIcon(wallet, index)}
                    </div>
                  </div>
                );

                if (!isDuplicate) {
                  previousWalletId = wallet.id;
                  rowNumber++;
                }

                return walletRow;
              })}
            </div>
            <div className={styles.action}>
              <Button
                className={buttonClasses}
                disabled={isDisabled}
                label={buttonLabel}
                onClick={onContinue}
                skin={ButtonSkin}
              />
              <div>
                <Link
                  className={styles.learnMoreLink}
                  onClick={onLinkClick}
                  label={linkLabel}
                  skin={LinkSkin}
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
        </div>
      </Dialog>
    );
  }
}

export default WalletSelectImportDialog;
