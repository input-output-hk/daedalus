import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import globalMessages from '../../../i18n/global-messages';
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../config/cryptoConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletBackupPrivacyWarningDi... Remove this comment to see the full error message
import styles from './WalletBackupPrivacyWarningDialog.scss';

const messages = defineMessages({
  recoveryPhraseInstructions1: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions1',
    defaultMessage:
      '!!!On the following screen, you will be given a list of {walletRecoveryPhraseWordCount}  words to write down on paper and keep in a safe place. This list of words is the wallet recovery phrase for the wallet you are creating.',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  recoveryPhraseInstructions2: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions2',
    defaultMessage:
      '!!!The simplest way to keep your wallet recovery phrase secure is to never store it digitally or online. If you decide to use an online service, such as a password manager with an encrypted database, it is your responsibility to make sure that you use it correctly.',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  recoveryPhraseInstructions3: {
    id: 'wallet.backup.privacy.warning.dialog.recoveryPhraseInstructions3',
    defaultMessage:
      '!!!<strong>Using your recovery phrase is the only way to recover your wallet if your computer is lost, broken, stolen, or stops working.</strong>',
    description:
      'Instructions for backing up wallet recovery phrase on dialog that displays wallet recovery phrase.',
  },
  buttonLabelContinue: {
    id: 'wallet.backup.privacy.warning.dialog.button.labelContinue',
    defaultMessage: '!!!Continue',
    description: 'Label for button "Continue" on wallet backup dialog',
  },
  termNobodyWatching: {
    id: 'wallet.backup.privacy.warning.dialog.checkbox.label.nobodyWatching',
    defaultMessage:
      '!!!I confirm that nobody can see my screen, because anyone who knows my recovery phrase will be able to spend the ada in my new wallet.',
    description:
      'Label for the checkbox on wallet backup dialog describing that nobody should be watching when recovery phrase is shown',
  },
});
type Props = {
  countdownRemaining: number;
  canPhraseBeShown: boolean;
  isPrivacyNoticeAccepted: boolean;
  onAcceptPrivacyNotice: (...args: Array<any>) => any;
  onContinue: (...args: Array<any>) => any;
  onCancelBackup: (...args: Array<any>) => any;
};

@observer
class WalletBackupPrivacyWarningDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      countdownRemaining,
      canPhraseBeShown,
      onAcceptPrivacyNotice,
      onCancelBackup,
      isPrivacyNoticeAccepted,
      onContinue,
    } = this.props;
    const countdownDisplay =
      countdownRemaining > 0 ? ` (${countdownRemaining})` : '';
    const dialogClasses = classnames([
      styles.component,
      'WalletBackupPrivacyWarningDialog',
    ]);
    const actions = [
      {
        label:
          intl.formatMessage(messages.buttonLabelContinue) + countdownDisplay,
        onClick: onContinue,
        disabled: !canPhraseBeShown,
        primary: true,
      },
    ];
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(globalMessages.recoveryPhraseDialogTitle)}
        actions={actions}
        closeOnOverlayClick={false}
        onClose={onCancelBackup}
        closeButton={<DialogCloseButton onClose={onCancelBackup} />}
      >
        <WalletRecoveryInstructions
          instructionsText={intl.formatMessage(
            messages.recoveryPhraseInstructions1,
            {
              walletRecoveryPhraseWordCount: WALLET_RECOVERY_PHRASE_WORD_COUNT,
            }
          )}
        />
        <WalletRecoveryInstructions
          instructionsText={intl.formatMessage(
            messages.recoveryPhraseInstructions2
          )}
        />
        <WalletRecoveryInstructions
          instructionsText={
            <FormattedHTMLMessage {...messages.recoveryPhraseInstructions3} />
          }
        />
        <div className={styles.checkbox}>
          <Checkbox
            label={intl.formatMessage(messages.termNobodyWatching)}
            onChange={onAcceptPrivacyNotice}
            checked={isPrivacyNoticeAccepted}
            skin={CheckboxSkin}
          />
        </div>
      </Dialog>
    );
  }
}

export default WalletBackupPrivacyWarningDialog;
