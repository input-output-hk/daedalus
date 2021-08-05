// @flow
import type { Node } from 'react';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import moment from 'moment';
import LocalizableError from '../../../i18n/LocalizableError';
import {
  IS_ICO_PUBLIC_KEY_SHARING_ENABLED,
  IS_WALLET_PUBLIC_KEY_SHARING_ENABLED,
  IS_WALLET_UNDELEGATION_ENABLED,
} from '../../../config/walletsConfig';
import BorderedBox from '../../widgets/BorderedBox';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import ReadOnlyInput from '../../widgets/forms/ReadOnlyInput';
import UndelegateWalletButton from './UndelegateWalletButton';
import DelegateWalletButton from './DelegateWalletButton';
import DeleteWalletButton from './DeleteWalletButton';
import UndelegateWalletConfirmationDialog from './UndelegateWalletConfirmationDialog';
import DeleteWalletConfirmationDialog from './DeleteWalletConfirmationDialog';
import ChangeSpendingPasswordDialog from './ChangeSpendingPasswordDialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletSettings.scss';
import WalletRecoveryPhraseVerificationWidget from './WalletRecoveryPhraseVerificationWidget';
import type { Locale } from '../../../../../common/types/locales.types';
import { momentLocales } from '../../../../../common/types/locales.types';
import ICOPublicKeyBox from './ICOPublicKeyBox';
import WalletPublicKeyBox from './WalletPublicKeyBox';
import ICOPublicKeyDialog from './ICOPublicKeyDialog';
import ICOPublicKeyQRCodeDialog from './ICOPublicKeyQRCodeDialog';
import WalletPublicKeyDialog from './WalletPublicKeyDialog';
import WalletPublicKeyQRCodeDialog from './WalletPublicKeyQRCodeDialog';
import getWalletSettingsMessages from './WalletSettings.messages';
import UndelegateWalletBox from './UndelegateWalletBox';

const messages = getWalletSettingsMessages();

type Props = {
  walletId: string,
  walletName: string,
  isRestoring: boolean,
  isSyncing: boolean,
  isDelegating: boolean,
  walletPublicKey: ?string,
  icoPublicKey: ?string,
  creationDate: Date,
  spendingPasswordUpdateDate: ?Date,
  error?: ?LocalizableError,
  openDialogAction: Function,
  isDialogOpen: Function,
  onFieldValueChange: Function,
  onStartEditing: Function,
  onStopEditing: Function,
  onCancel: Function,
  onVerifyRecoveryPhrase: Function,
  onCopyWalletPublicKey: Function,
  onCopyICOPublicKey: Function,
  updateDataForActiveDialogAction: Function,
  onDelegateClick: Function,
  nameValidator: Function,
  isLegacy: boolean,
  changeSpendingPasswordDialog: Node,
  walletPublicKeyDialogContainer: Node,
  icoPublicKeyDialogContainer: Node,
  walletPublicKeyQRCodeDialogContainer: Node,
  icoPublicKeyQRCodeDialogContainer: Node,
  undelegateWalletDialogContainer: Node,
  deleteWalletDialogContainer: Node,
  shouldDisplayRecoveryPhrase: boolean,
  recoveryPhraseVerificationDate: ?Date,
  recoveryPhraseVerificationStatus: string,
  recoveryPhraseVerificationStatusType: string,
  wordCount: number,
  locale: Locale,
  isSpendingPasswordSet: boolean,
  isHardwareWallet: boolean,
};

type State = {
  isFormBlocked: boolean,
};

@observer
export default class WalletSettings extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isFormBlocked: false,
  };

  componentDidUpdate() {
    const { isDialogOpen } = this.props;
    const { isFormBlocked } = this.state;
    // Set "name" input to active and "unblock form" on Dialog close
    if (
      !isDialogOpen(DeleteWalletConfirmationDialog) &&
      !isDialogOpen(ChangeSpendingPasswordDialog) &&
      isFormBlocked
    ) {
      this.unblockForm();
    }
  }

  componentWillUnmount() {
    // This call is used to prevent display of old successfully-updated messages
    this.props.onCancel();
  }

  onBlockForm = () => {
    this.setState({ isFormBlocked: true });
  };

  unblockForm = () => {
    this.setState({ isFormBlocked: false });
  };

  onUndelegateWalletClick = async () => {
    const {
      walletId,
      openDialogAction,
      updateDataForActiveDialogAction,
    } = this.props;
    this.onBlockForm();
    openDialogAction({
      dialog: UndelegateWalletConfirmationDialog,
    });
    updateDataForActiveDialogAction({
      data: { walletId },
    });
  };

  renderUndelegateWalletBox = () => {
    const { intl } = this.context;
    const {
      isDelegating,
      isRestoring,
      isSyncing,
      isLegacy,
      isDialogOpen,
      onDelegateClick,
      undelegateWalletDialogContainer,
    } = this.props;

    /// @TODO: Once undelegation for rewarded wallet works fine with api, remove reward checking and config
    if (!IS_WALLET_UNDELEGATION_ENABLED || isLegacy) {
      return null;
    }

    let headerMessage = null;
    let warningMessage = null;

    if (isDelegating) {
      headerMessage = intl.formatMessage(messages.undelegateWalletHeader);
      warningMessage =
        isRestoring || isSyncing
          ? intl.formatMessage(messages.undelegateWalletDisabledWarning)
          : intl.formatMessage(messages.undelegateWalletWarning);
    } else {
      headerMessage = intl.formatMessage(messages.delegateWalletHeader);
      warningMessage =
        isRestoring || isSyncing
          ? intl.formatMessage(messages.delegateWalletDisabledWarning)
          : intl.formatMessage(messages.delegateWalletWarning);
    }

    return (
      <>
        <BorderedBox className={styles.undelegateWalletBox}>
          <span>{headerMessage}</span>
          <div className={styles.contentBox}>
            <div>
              <p>{warningMessage}</p>
            </div>
            {isDelegating ? (
              <UndelegateWalletButton
                disabled={isRestoring || isSyncing}
                onUndelegate={this.onUndelegateWalletClick}
              />
            ) : (
              <DelegateWalletButton
                disabled={isRestoring || isSyncing}
                onDelegate={onDelegateClick}
              />
            )}
          </div>
        </BorderedBox>
        {isDialogOpen(UndelegateWalletConfirmationDialog)
          ? undelegateWalletDialogContainer
          : false}
      </>
    );
  };

  renderDeleteWalletBox = () => {
    const { intl } = this.context;
    const {
      openDialogAction,
      isDialogOpen,
      deleteWalletDialogContainer,
    } = this.props;

    return (
      <>
        <BorderedBox className={styles.deleteWalletBox}>
          <span>{intl.formatMessage(messages.deleteWalletHeader)}</span>
          <div className={styles.contentBox}>
            <div>
              <p>{intl.formatMessage(messages.deleteWalletWarning1)}</p>
              <p>{intl.formatMessage(messages.deleteWalletWarning2)}</p>
            </div>
            <DeleteWalletButton
              onClick={() => {
                this.onBlockForm();
                openDialogAction({
                  dialog: DeleteWalletConfirmationDialog,
                });
              }}
            />
          </div>
        </BorderedBox>
        {isDialogOpen(DeleteWalletConfirmationDialog)
          ? deleteWalletDialogContainer
          : false}
      </>
    );
  };

  render() {
    const { intl } = this.context;
    const {
      isDelegating,
      isRestoring,
      isSyncing,
      undelegateWalletDialogContainer,
      walletId,
      onDelegateClick,
      updateDataForActiveDialogAction,
      walletName,
      creationDate,
      spendingPasswordUpdateDate,
      error,
      isDialogOpen,
      openDialogAction,
      onFieldValueChange,
      onStartEditing,
      onStopEditing,
      onCancel,
      onVerifyRecoveryPhrase,
      nameValidator,
      isLegacy,
      changeSpendingPasswordDialog,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
      locale,
      isSpendingPasswordSet,
      isHardwareWallet,
      shouldDisplayRecoveryPhrase,
      wordCount,
      walletPublicKeyDialogContainer,
      walletPublicKeyQRCodeDialogContainer,
      icoPublicKeyDialogContainer,
      icoPublicKeyQRCodeDialogContainer,
    } = this.props;
    const { isFormBlocked } = this.state;

    // Set Japanese locale to moment. Default is en-US
    moment.locale(momentLocales[locale]);

    const passwordMessage = isSpendingPasswordSet
      ? intl.formatMessage(messages.passwordLastUpdated, {
          lastUpdated: moment(spendingPasswordUpdateDate)
            .locale(this.context.intl.locale)
            .fromNow(),
        })
      : intl.formatMessage(messages.passwordNotSet);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <InlineEditingInput
            className="walletName"
            label={intl.formatMessage(messages.name)}
            value={walletName}
            maxLength={40}
            onFocus={() => onStartEditing('name')}
            onBlur={onStopEditing}
            onCancel={onCancel}
            onSubmit={(value) => onFieldValueChange('name', value)}
            isValid={nameValidator}
            valueErrorMessage={intl.formatMessage(
              globalMessages.invalidWalletName
            )}
            readOnly={isFormBlocked}
          />

          {!isHardwareWallet && (
            <ReadOnlyInput
              label={intl.formatMessage(messages.passwordLabel)}
              value={passwordMessage}
              isSet={isSpendingPasswordSet}
              withButton
              onClick={() => {
                this.onBlockForm();
                openDialogAction({
                  dialog: ChangeSpendingPasswordDialog,
                });
              }}
            />
          )}

          {shouldDisplayRecoveryPhrase && (
            <WalletRecoveryPhraseVerificationWidget
              onVerify={onVerifyRecoveryPhrase}
              recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
              recoveryPhraseVerificationStatus={
                recoveryPhraseVerificationStatus
              }
              recoveryPhraseVerificationStatusType={
                recoveryPhraseVerificationStatusType
              }
              creationDate={creationDate}
              locale={locale}
              wordCount={wordCount}
              isLegacy={isLegacy}
            />
          )}

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
        </BorderedBox>

        {isDialogOpen(ChangeSpendingPasswordDialog)
          ? changeSpendingPasswordDialog
          : false}

        {IS_WALLET_PUBLIC_KEY_SHARING_ENABLED && !isLegacy && (
          <>
            <WalletPublicKeyBox
              publicKey={this.props.walletPublicKey}
              locale={this.props.locale}
              onCopyWalletPublicKey={this.props.onCopyWalletPublicKey}
              openDialogAction={this.props.openDialogAction}
            />
            {isDialogOpen(WalletPublicKeyDialog) &&
              walletPublicKeyDialogContainer}
            {isDialogOpen(WalletPublicKeyQRCodeDialog) &&
              walletPublicKeyQRCodeDialogContainer}
          </>
        )}

        {IS_ICO_PUBLIC_KEY_SHARING_ENABLED && !isLegacy && !isHardwareWallet && (
          <>
            <ICOPublicKeyBox
              publicKey={this.props.icoPublicKey}
              locale={this.props.locale}
              onCopyICOPublicKey={this.props.onCopyICOPublicKey}
              openDialogAction={this.props.openDialogAction}
            />
            {isDialogOpen(ICOPublicKeyDialog) && icoPublicKeyDialogContainer}
            {isDialogOpen(ICOPublicKeyQRCodeDialog) &&
              icoPublicKeyQRCodeDialogContainer}
          </>
        )}

        {IS_WALLET_UNDELEGATION_ENABLED && !isLegacy && (
          <UndelegateWalletBox
            {...{
              isDelegating,
              isRestoring,
              isSyncing,
              isDialogOpen,
              undelegateWalletDialogContainer,
              walletId,
              onBlockForm: this.onBlockForm,
              onDelegateClick,
              openDialogAction,
              updateDataForActiveDialogAction,
            }}
          />
        )}
        {this.renderDeleteWalletBox()}
      </div>
    );
  }
}
