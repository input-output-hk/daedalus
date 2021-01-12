// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { WALLET_RECOVERY_PHRASE_WORD_COUNT } from '../../../config/cryptoConfig';
import suggestedMnemonics from '../../../../../common/config/crypto/valid-words.en';
import { isValidMnemonic } from '../../../../../common/config/crypto/decrypt';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import {
  errorOrIncompleteMarker,
  validateMnemonics,
} from '../../../utils/validations';
import WalletRecoveryPhraseMnemonic from './WalletRecoveryPhraseMnemonic';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import WalletRecoveryInstructions from './WalletRecoveryInstructions';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletRecoveryPhraseEntryDialog.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';

const messages = defineMessages({
  verificationInstructions: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.verification.instructions',
    defaultMessage:
      '!!!Please enter your {wordCount}-word wallet recovery phrase. Make sure you enter the words in the correct order.',
    description:
      'Instructions for verifying wallet recovery phrase on dialog for entering wallet recovery phrase.',
  },
  recoveryPhraseInputLabel: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInputLabel',
    defaultMessage: '!!!Verify your recovery phrase',
    description:
      'Label for the recovery phrase input on dialog for entering wallet recovery phrase.',
  },
  recoveryPhraseInputHint: {
    id: 'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInputHint',
    defaultMessage: '!!!Enter your {numberOfWords}-word recovery phrase',
    description: 'Placeholder for the mnemonics autocomplete.',
  },
  recoveryPhraseNoResults: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInputNoResults',
    defaultMessage: '!!!No results',
    description:
      '"No results" message for the recovery phrase input search results.',
  },
  recoveryPhraseInvalidMnemonics: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.recoveryPhraseInvalidMnemonics',
    defaultMessage: '!!!Invalid recovery phrase',
    description:
      'Error message shown when invalid recovery phrase was entered.',
  },
  buttonLabelConfirm: {
    id: 'wallet.recovery.phrase.show.entry.dialog.button.labelConfirm',
    defaultMessage: '!!!Confirm',
    description: 'Label for button "Confirm" on wallet backup dialog',
  },
  termOffline: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.offline',
    defaultMessage:
      '!!!I understand that the simplest way to keep my wallet recovery phrase secure is to never store it digitally or online. If I decide to use an online service, such as a password manager with an encrypted database, it is my responsibility to make sure that I use it correctly.',
    description: 'Term on wallet creation to store recovery phrase offline',
  },
  termRecovery: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.recovery',
    defaultMessage:
      '!!!I understand that the only way to recover my wallet if my computer is lost, broken, stolen, or stops working is to use my wallet recovery phrase.',
    description:
      'Term and condition on wallet backup dialog describing that wallet can only be recovered with a security phrase',
  },
  termRewards: {
    id:
      'wallet.backup.recovery.phrase.entry.dialog.terms.and.condition.rewards',
    defaultMessage: `!!!<strong>I understand that I will need the wallet recovery phrase of this wallet to receive my Incentivized Testnet ada rewards on the Cardano mainnet.</strong>`,
    description:
      'Term and condition on wallet backup dialog describing that wallet can only be recovered with a security phrase',
  },
});

const { isIncentivizedTestnet } = global;

type Props = {
  enteredPhrase: Array<string>,
  isValid: boolean,
  isTermOfflineAccepted: boolean,
  isTermRecoveryAccepted: boolean,
  isTermRewardsAccepted: boolean,
  isSubmitting: boolean,
  onUpdateVerificationPhrase: Function,
  canFinishBackup: boolean,
  onAcceptTermOffline: Function,
  onAcceptTermRecovery: Function,
  onAcceptTermRewards: Function,
  onRestartBackup: Function,
  onCancelBackup: Function,
  onFinishBackup: Function,
};

@observer
export default class WalletRecoveryPhraseEntryDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        recoveryPhrase: {
          value: [],
          validators: ({ field }) => {
            const enteredWords = field.value;
            this.props.onUpdateVerificationPhrase({
              verificationPhrase: enteredWords,
            });
            return validateMnemonics({
              requiredWords: WALLET_RECOVERY_PHRASE_WORD_COUNT,
              providedWords: field.value,
              validator: () => [
                isValidMnemonic(enteredWords.join(' '), enteredWords.length),
                this.context.intl.formatMessage(
                  messages.recoveryPhraseInvalidMnemonics
                ),
              ],
            });
          },
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
      },
    }
  );

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      enteredPhrase,
      isValid,
      isTermOfflineAccepted,
      isTermRecoveryAccepted,
      isTermRewardsAccepted,
      isSubmitting,
      onAcceptTermOffline,
      onAcceptTermRecovery,
      onAcceptTermRewards,
      canFinishBackup,
      onRestartBackup,
      onCancelBackup,
      onFinishBackup,
    } = this.props;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const dialogClasses = classnames([
      styles.component,
      'WalletRecoveryPhraseEntryDialog',
    ]);
    const wordCount = WALLET_RECOVERY_PHRASE_WORD_COUNT;
    const enteredPhraseString = enteredPhrase.join(' ');

    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.buttonLabelConfirm)
    ) : (
      <LoadingSpinner />
    );

    const actions = [
      {
        label: buttonLabel,
        onClick: onFinishBackup,
        disabled: !canFinishBackup,
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
        backButton={
          !isValid ? <DialogBackButton onBack={onRestartBackup} /> : null
        }
      >
        {!isValid && (
          <>
            <WalletRecoveryInstructions
              instructionsText={intl.formatMessage(
                messages.verificationInstructions,
                {
                  wordCount,
                }
              )}
            />

            <Autocomplete
              {...recoveryPhraseField.bind()}
              label={intl.formatMessage(messages.recoveryPhraseInputLabel)}
              placeholder={intl.formatMessage(
                messages.recoveryPhraseInputHint,
                {
                  numberOfWords: wordCount,
                }
              )}
              options={suggestedMnemonics}
              requiredSelections={[wordCount]}
              requiredSelectionsInfo={(required, actual) =>
                intl.formatMessage(globalMessages.knownMnemonicWordCount, {
                  actual,
                  required,
                })
              }
              maxSelections={wordCount}
              error={errorOrIncompleteMarker(recoveryPhraseField.error)}
              maxVisibleOptions={5}
              noResultsMessage={intl.formatMessage(
                messages.recoveryPhraseNoResults
              )}
              skin={AutocompleteSkin}
              optionHeight={50}
            />
          </>
        )}

        {isValid && (
          <>
            <WalletRecoveryPhraseMnemonic phrase={enteredPhraseString} />
            <div>
              <div className={styles.checkbox}>
                <Checkbox
                  label={<FormattedHTMLMessage {...messages.termOffline} />}
                  onChange={onAcceptTermOffline}
                  checked={isTermOfflineAccepted}
                  skin={CheckboxSkin}
                />
              </div>
              <div className={styles.checkbox}>
                <Checkbox
                  className={isIncentivizedTestnet ? '' : styles.isBold}
                  label={intl.formatMessage(messages.termRecovery)}
                  onChange={onAcceptTermRecovery}
                  checked={isTermRecoveryAccepted}
                  skin={CheckboxSkin}
                />
              </div>
              {isIncentivizedTestnet && (
                <div className={styles.checkbox}>
                  <Checkbox
                    label={<FormattedHTMLMessage {...messages.termRewards} />}
                    onChange={onAcceptTermRewards}
                    checked={isTermRewardsAccepted}
                    skin={CheckboxSkin}
                  />
                </div>
              )}
            </div>
          </>
        )}
      </Dialog>
    );
  }
}
