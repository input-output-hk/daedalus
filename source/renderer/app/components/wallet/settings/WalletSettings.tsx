// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
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
import UndelegateWalletConfirmationDialog from './UndelegateWalletConfirmationDialog';
import WalletSettingsActionConfirmationDialog from './WalletSettingsRemoveConfirmationDialog';
import UnpairWallet from './UnpairWallet';
import DeleteWallet from './DeleteWallet';
import ChangeSpendingPasswordDialog from './ChangeSpendingPasswordDialog';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSettings.scss' or its ... Remove this comment to see the full error message
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
import type { ReactIntlMessage } from '../../../types/i18nTypes';

export const messages: Record<string, ReactIntlMessage> = defineMessages({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description:
      'Label for the "Transaction assurance security level" dropdown.',
  },
  undelegateWalletHeader: {
    id: 'wallet.settings.undelegateWallet.header',
    defaultMessage: '!!!Undelegating your wallet',
    description: 'Undelegate wallet header on the wallet settings page.',
  },
  undelegateWalletWarning: {
    id: 'wallet.settings.undelegateWallet.warning',
    defaultMessage:
      '!!!If you are planning to stop using this wallet and remove all funds, you should first undelegate it to recover your 2 ada deposit. You will continue getting delegation rewards during the three Cardano epochs after undelegating your wallet.',
    description: 'Undelegate wallet warning explaining the consequences.',
  },
  undelegateWalletDisabledWarning: {
    id: 'wallet.settings.undelegateWallet.disabledWarning',
    defaultMessage:
      "!!!This wallet is synchronizing with the blockchain, so this wallet's delegation status is currently unknown, and undelegation is not possible.",
    description:
      'Undelegate wallet disabled warning explaining why it is disabled.',
  },
  delegateWalletHeader: {
    id: 'wallet.settings.delegateWallet.header',
    defaultMessage: '!!!Delegate your wallet',
    description: 'Delegate wallet header on the wallet settings page.',
  },
  delegateWalletWarning: {
    id: 'wallet.settings.delegateWallet.warning',
    defaultMessage:
      "!!!This wallet is not delegated. Please, delegate the stake from this wallet to earn rewards and support the Cardano network's security.",
    description: 'Delegate wallet warning.',
  },
  delegateWalletDisabledWarning: {
    id: 'wallet.settings.delegateWallet.disabledWarning',
    defaultMessage:
      "!!!This wallet is synchronizing with the blockchain, so this wallet's delegation status is currently unknown, and delegation is not possible.",
    description:
      'Delegate wallet disabled warning explaining why it is disabled.',
  },
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
  },
  passwordLabel: {
    id: 'wallet.settings.password',
    defaultMessage: '!!!Password',
    description: 'Label for the "Password" field.',
  },
  passwordLastUpdated: {
    id: 'wallet.settings.passwordLastUpdated',
    defaultMessage: '!!!Last updated',
    description: 'Last updated X time ago message.',
  },
  passwordNotSet: {
    id: 'wallet.settings.passwordNotSet',
    defaultMessage: "!!!You still don't have password",
    description: "You still don't have password set message.",
  },
});
type Props = {
  walletId: string;
  walletName: string;
  isRestoring: boolean;
  isSyncing: boolean;
  isDelegating: boolean;
  walletPublicKey: string | null | undefined;
  icoPublicKey: string | null | undefined;
  creationDate: Date;
  spendingPasswordUpdateDate: Date | null | undefined;
  error?: LocalizableError | null | undefined;
  openDialogAction: (...args: Array<any>) => any;
  isDialogOpen: (...args: Array<any>) => any;
  onFieldValueChange: (...args: Array<any>) => any;
  onStartEditing: (...args: Array<any>) => any;
  onStopEditing: (...args: Array<any>) => any;
  onCancel: (...args: Array<any>) => any;
  onVerifyRecoveryPhrase: (...args: Array<any>) => any;
  onCopyWalletPublicKey: (...args: Array<any>) => any;
  onCopyICOPublicKey: (...args: Array<any>) => any;
  updateDataForActiveDialogAction: (...args: Array<any>) => any;
  onDelegateClick: (...args: Array<any>) => any;
  nameValidator: (...args: Array<any>) => any;
  isLegacy: boolean;
  changeSpendingPasswordDialog: Node;
  walletPublicKeyDialogContainer: Node;
  icoPublicKeyDialogContainer: Node;
  walletPublicKeyQRCodeDialogContainer: Node;
  icoPublicKeyQRCodeDialogContainer: Node;
  undelegateWalletDialogContainer: Node;
  deleteWalletDialogContainer: Node;
  unpairWalletDialogContainer: Node;
  shouldDisplayRecoveryPhrase: boolean;
  recoveryPhraseVerificationDate: Date | null | undefined;
  recoveryPhraseVerificationStatus: string;
  recoveryPhraseVerificationStatusType: string;
  wordCount: number;
  locale: Locale;
  isSpendingPasswordSet: boolean;
  isHardwareWallet: boolean;
};
type State = {
  isFormBlocked: boolean;
};

@observer
class WalletSettings extends Component<Props, State> {
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
      !isDialogOpen(WalletSettingsActionConfirmationDialog) &&
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
    this.setState({
      isFormBlocked: true,
    });
  };
  unblockForm = () => {
    this.setState({
      isFormBlocked: false,
    });
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
      data: {
        walletId,
      },
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

    let headerMessage;
    let warningMessage;

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

  render() {
    const { intl } = this.context;
    const {
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
      deleteWalletDialogContainer,
      unpairWalletDialogContainer,
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
      <div className={styles.root}>
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
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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

        {this.renderUndelegateWalletBox()}
        {isHardwareWallet ? (
          <UnpairWallet
            openDialogAction={openDialogAction}
            isDialogOpen={isDialogOpen}
            unpairWalletDialogContainer={unpairWalletDialogContainer}
            onBlockForm={this.onBlockForm}
          />
        ) : (
          <DeleteWallet
            openDialogAction={openDialogAction}
            isDialogOpen={isDialogOpen}
            deleteWalletDialogContainer={deleteWalletDialogContainer}
            onBlockForm={this.onBlockForm}
          />
        )}
      </div>
    );
  }
}

export default WalletSettings;
