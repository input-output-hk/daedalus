import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { InvalidMnemonicError } from '../../../i18n/errors';
import globalMessages from '../../../i18n/global-messages';
import styles from './VerificationDialog.scss';
import {
  PAPER_WALLET_PRINTED_WORDS_COUNT,
  PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  PAPER_WALLET_WRITTEN_WORDS_COUNT,
} from '../../../config/cryptoConfig';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { MnemonicInput } from '../mnemonic-input';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.verification.dialog.headline',
    defaultMessage: '!!!Verify certificate',
    description:
      'Headline for the "Paper wallet create certificate verification dialog".',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.verification.dialog.subtitle',
    defaultMessage:
      '!!!Enter your paper wallet recovery phrase to verify your paper wallet certificate.',
    description:
      '"Paper wallet create certificate verification dialog" subtitle.',
  },
  instructions: {
    id: 'paper.wallet.create.certificate.verification.dialog.instructions',
    defaultMessage: `!!!Make sure you enter all {fullPhraseWordCount} words for the paper wallet recovery phrase,
     first {printedWordCount} words printed on the certificate followed by the {writtenWordCount} words you wrote by hand.`,
    description:
      '"Paper wallet create certificate verification dialog" subtitle.',
  },
  recoveryPhraseLabel: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.label',
    defaultMessage: '!!!Paper wallet recovery phrase',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase label.',
  },
  clearButtonLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.button.clearLabel',
    defaultMessage: '!!!Clear',
    description:
      '"Paper wallet create certificate verification dialog" button clear label.',
  },
  storingUnderstandanceLabel: {
    id:
      'paper.wallet.create.certificate.verification.dialog.storingUnderstandanceConfirmationLabel',
    defaultMessage:
      '!!!I understand that the paper wallet I create will not be stored in Daedalus.',
    description:
      '"Paper wallet create certificate verification dialog" storing understandance confirmation.',
  },
  recoveringUnderstandanceLabel: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveringUnderstandanceConfirmationLabel',
    defaultMessage:
      '!!!I understand that my paper wallet can be recovered only by using my paper wallet certificate.',
    description:
      '"Paper wallet create certificate verification dialog" recovering understandance confirmation.',
  },
});
type State = {
  storingConfirmed: boolean;
  recoveringConfirmed: boolean;
  isRecoveryPhraseValid: boolean;
  error: boolean;
};
type Props = {
  walletCertificateRecoveryPhrase: string;
  additionalMnemonicWords: string;
  suggestedMnemonics: Array<string>;
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
};

interface FormFields {
  recoveryPhrase: string;
}

@observer
class VerificationDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    storingConfirmed: false,
    recoveringConfirmed: false,
    isRecoveryPhraseValid: false,
    error: false,
  };
  onStoringConfirmation = () => {
    this.setState((prevState) => ({
      storingConfirmed: !prevState.storingConfirmed,
    }));
  };
  onRecoveringConfirmation = () => {
    this.setState((prevState) => ({
      recoveringConfirmed: !prevState.recoveringConfirmed,
    }));
  };
  form = new ReactToolboxMobxForm<FormFields>(
    {
      fields: {
        recoveryPhrase: {
          label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
          value: [],
          validators: [
            ({ field }) => {
              const { intl } = this.context;
              const {
                walletCertificateRecoveryPhrase,
                additionalMnemonicWords,
              } = this.props;
              const { storingConfirmed, recoveringConfirmed } = this.state;
              const enteredWordsArray = field.value;

              if (
                enteredWordsArray.length <
                PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT
              ) {
                // If user hasn't entered all words of the paper wallet recovery phrase yet
                return [
                  false,
                  intl.formatMessage(globalMessages.incompleteMnemonic, {
                    expected: PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                  }),
                ];
              }

              const fullRecoveryPhrase = `${walletCertificateRecoveryPhrase} ${additionalMnemonicWords}`;
              const enteredRecoveryPhrase = enteredWordsArray.join(' ');
              const isRecoveryPhraseValid =
                fullRecoveryPhrase === enteredRecoveryPhrase;
              this.setState({
                isRecoveryPhraseValid,
                // disabled and uncheck confirmation checkboxes if recovery phrase is not valid
                storingConfirmed: isRecoveryPhraseValid
                  ? storingConfirmed
                  : false,
                recoveringConfirmed: isRecoveryPhraseValid
                  ? recoveringConfirmed
                  : false,
              });
              return [
                isRecoveryPhraseValid,
                this.context.intl.formatMessage(new InvalidMnemonicError()),
              ];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        showErrorsOnChange: false,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  handleMnemonicInputChange = (value) => {
    if (this.state.error) {
      this.setState((prevState) => ({ ...prevState, error: false }));
    }

    this.form.$('recoveryPhrase').bind().onChange(value);
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { recoveryPhrase } = form.values();
        this.props.onContinue({
          recoveryPhrase,
        });
      },
      onError: () => {
        this.setState((prevState) => ({ ...prevState, error: true }));
      },
    });
  };
  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each((field) => {
      field.debouncedValidation.cancel();
    });
    form.reset();
    form.showErrors(false);

    this.setState({
      storingConfirmed: false,
      recoveringConfirmed: false,
    });
  };

  render() {
    const { intl } = this.context;
    const { form, resetForm } = this;
    const { suggestedMnemonics, onClose } = this.props;
    const {
      storingConfirmed,
      recoveringConfirmed,
      isRecoveryPhraseValid,
    } = this.state;
    const recoveryPhraseField = form.$('recoveryPhrase');
    const dialogClasses = classnames([styles.dialog, 'verificationDialog']);
    const storingUnderstandanceCheckboxClasses = classnames([
      styles.checkbox,
      'storingUnderstandance',
    ]);
    const recoveringUnderstandanceCheckboxClasses = classnames([
      styles.checkbox,
      'recoveringUnderstandance',
    ]);
    const actions = [
      {
        className: 'clearButton',
        label: intl.formatMessage(messages.clearButtonLabel),
        onClick: resetForm.bind(this),
      },
      {
        className: 'continueButton',
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        disabled: form.hasError,
        onClick: this.submit.bind(this),
      },
    ];
    const { reset, ...mnemonicInputProps } = recoveryPhraseField.bind();

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.verificationContentWrapper}>
          <p className={styles.subtitle}>
            {intl.formatMessage(messages.subtitle)}
          </p>
          <p className={styles.instructions}>
            <strong>
              {intl.formatMessage(messages.instructions, {
                fullPhraseWordCount: PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
                printedWordCount: PAPER_WALLET_PRINTED_WORDS_COUNT,
                writtenWordCount: PAPER_WALLET_WRITTEN_WORDS_COUNT,
              })}
            </strong>
          </p>
          <div className={styles.content}>
            <MnemonicInput
              {...mnemonicInputProps}
              onChange={this.handleMnemonicInputChange}
              availableWords={suggestedMnemonics.sort()}
              wordCount={PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT}
              error={recoveryPhraseField.error}
              reset={form.resetting}
            />
            <Checkbox
              className={storingUnderstandanceCheckboxClasses}
              label={intl.formatMessage(messages.storingUnderstandanceLabel)}
              onChange={this.onStoringConfirmation}
              checked={storingConfirmed}
              disabled={!isRecoveryPhraseValid}
              skin={CheckboxSkin}
            />

            <Checkbox
              className={recoveringUnderstandanceCheckboxClasses}
              label={intl.formatMessage(messages.recoveringUnderstandanceLabel)}
              onChange={this.onRecoveringConfirmation}
              checked={recoveringConfirmed}
              disabled={!isRecoveryPhraseValid}
              skin={CheckboxSkin}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}

export default VerificationDialog;
