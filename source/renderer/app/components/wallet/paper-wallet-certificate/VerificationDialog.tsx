import React, { Component } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Autocomplete } from 'react-polymorph/lib/components/Autocomplete';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { AutocompleteSkin } from 'react-polymorph/lib/skins/simple/AutocompleteSkin';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { InvalidMnemonicError } from '../../../i18n/errors';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VerificationDialog.scss' or ... Remove this comment to see the full error message
import styles from './VerificationDialog.scss';
import {
  PAPER_WALLET_PRINTED_WORDS_COUNT,
  PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  PAPER_WALLET_WRITTEN_WORDS_COUNT,
} from '../../../config/cryptoConfig';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

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
  recoveryPhraseHint: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase hint.',
  },
  recoveryPhraseNoResults: {
    id:
      'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.noResults',
    defaultMessage: '!!!No results',
    description:
      '"Paper wallet create certificate verification dialog" recovery phrase no results label.',
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
};
type Props = {
  walletCertificateRecoveryPhrase: string;
  additionalMnemonicWords: string;
  suggestedMnemonics: Array<string>;
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
};

@observer
class VerificationDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    storingConfirmed: false,
    recoveringConfirmed: false,
    isRecoveryPhraseValid: false,
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
  recoveryPhraseAutocomplete: Autocomplete;
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        recoveryPhrase: {
          label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
          placeholder: this.context.intl.formatMessage(
            messages.recoveryPhraseHint
          ),
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
              const enteredRecoveryPhrase = join(enteredWordsArray, ' ');
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
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { recoveryPhrase } = form.values();
        this.props.onContinue({
          recoveryPhrase,
        });
      },
      onError: () => {},
    });
  };
  resetForm = () => {
    const { form } = this;
    const autocomplete = this.recoveryPhraseAutocomplete;
    // Cancel all debounced field validations
    // @ts-ignore ts-migrate(2339) FIXME: Property 'each' does not exist on type 'ReactToolb... Remove this comment to see the full error message
    form.each((field) => {
      field.debouncedValidation.cancel();
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'reset' does not exist on type 'ReactTool... Remove this comment to see the full error message
    form.reset();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'showErrors' does not exist on type 'Reac... Remove this comment to see the full error message
    form.showErrors(false);
    // Autocomplete has to be reset manually
    autocomplete.clear();

    if (autocomplete && autocomplete.focus) {
      autocomplete.focus();
    }

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
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
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
        disabled: !storingConfirmed || !recoveringConfirmed,
        onClick: this.submit.bind(this),
      },
    ];
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
            <Autocomplete
              className={styles.recoveryPhrase}
              options={suggestedMnemonics}
              maxSelections={PAPER_WALLET_RECOVERY_PHRASE_WORD_COUNT}
              ref={(autocomplete) => {
                this.recoveryPhraseAutocomplete = autocomplete;
              }}
              {...recoveryPhraseField.bind()}
              error={recoveryPhraseField.error}
              maxVisibleOptions={5}
              noResultsMessage={intl.formatMessage(
                messages.recoveryPhraseNoResults
              )}
              skin={AutocompleteSkin}
              optionHeight={50}
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
