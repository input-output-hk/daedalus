// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Autocomplete from 'react-polymorph/lib/components/Autocomplete';
import SimpleAutocompleteSkin from 'react-polymorph/lib/skins/simple/raw/AutocompleteSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { InvalidMnemonicError } from '../../../i18n/errors';
import globalMessages from '../../../i18n/global-messages';
import styles from './VerificationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.verification.dialog.headline',
    defaultMessage: '!!!Verify certificate',
    description: 'Headline for the "Paper wallet create certificate verification dialog".'
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.verification.dialog.subtitle',
    defaultMessage: '!!!Enter your paper wallet recovery phrase to verify your paper wallet certificate.',
    description: '"Paper wallet create certificate verification dialog" subtitle.'
  },
  recoveryPhraseLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.label',
    defaultMessage: '!!!Paper wallet recovery phrase',
    description: '"Paper wallet create certificate verification dialog" recovery phrase label.'
  },
  recoveryPhraseHint: {
    id: 'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.hint',
    defaultMessage: '!!!Enter recovery phrase',
    description: '"Paper wallet create certificate verification dialog" recovery phrase hint.'
  },
  recoveryPhraseNoResults: {
    id: 'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.noResults',
    defaultMessage: '!!!No results',
    description: '"Paper wallet create certificate verification dialog" recovery phrase no results label.'
  },
  clearButtonLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.button.clearLabel',
    defaultMessage: '!!!Clear',
    description: '"Paper wallet create certificate verification dialog" button clear label.'
  },
  storingUnderstandanceLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.storingUnderstandanceConfirmationLabel',
    defaultMessage: '!!!I understand that the paper wallet I create will not be stored in Daedalus.',
    description: '"Paper wallet create certificate verification dialog" storing understandance confirmation.'
  },
  recoveringUnderstandanceLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.recoveringUnderstandanceConfirmationLabel',
    defaultMessage: '!!!I understand that my wallet can only be recovered using my paper wallet certificate.',
    description: '"Paper wallet create certificate verification dialog" recovering understandance confirmation.'
  },
  errorMessage: {
    id: 'paper.wallet.create.certificate.verification.dialog.errorMessage',
    defaultMessage: `!!!Invalid password or shielded recovery phrase / password combination.<br/>
      Make sure you enter the shielded recovery phrase and the password from the certificate.
      Your certificate should not be used without passing this validation step.`,
    description: '"Paper wallet create certificate verification dialog" error message when password or recovery phrase are invalid.',
  }
});

type State = {
  storingConfirmed: boolean,
  recoveringConfirmed: boolean,
  isRecoveryPhraseValid: boolean,
};

type Props = {
  walletCertificateRecoveryPhrase: string,
  additionalMnemonicWords: string,
  error: boolean,
  suggestedMnemonics: Array<string>,
  onContinue: Function,
};

@observer
export default class VerificationDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    storingConfirmed: false,
    recoveringConfirmed: false,
    isRecoveryPhraseValid: false,
  };

  recoveryPhraseAutocomplete: Autocomplete;

  form = new ReactToolboxMobxForm({
    fields: {
      recoveryPhrase: {
        label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
        placeholder: this.context.intl.formatMessage(messages.recoveryPhraseHint),
        value: '',
        validators: [({ field }) => {
          const { walletCertificateRecoveryPhrase, additionalMnemonicWords } = this.props;
          const {
            storingConfirmed,
            recoveringConfirmed,
          } = this.state;

          const value = join(field.value, ' ');
          const fullRecoveryPhrase = `${walletCertificateRecoveryPhrase} ${additionalMnemonicWords}`;
          if (value === '') return [false, this.context.intl.formatMessage(globalMessages.fieldIsRequired)];
          const isRecoveryPhraseValid = fullRecoveryPhrase === value;
          this.setState({
            isRecoveryPhraseValid,
            // disabled and uncheck confirmation checkboxes if recovery phrase is not valid
            storingConfirmed: isRecoveryPhraseValid ? storingConfirmed : false,
            recoveringConfirmed: isRecoveryPhraseValid ? recoveringConfirmed : false,
          });
          return [
            isRecoveryPhraseValid,
            this.context.intl.formatMessage(new InvalidMnemonicError())
          ];
        }],
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { recoveryPhrase } = form.values();
        this.props.onContinue({ recoveryPhrase });
      },
      onError: () => {},
    });
  };

  resetForm = () => {
    const { form } = this;
    // Cancel all debounced field validations
    form.each((field) => { field.debouncedValidation.cancel(); });
    form.reset();
    form.showErrors(false);

    // Autocomplete has to be reset manually
    this.recoveryPhraseAutocomplete.clear();

    this.setState({
      storingConfirmed: false,
      recoveringConfirmed: false,
    });
  };

  render() {
    const { intl } = this.context;
    const { form, resetForm } = this;
    const { suggestedMnemonics, error } = this.props;
    const { storingConfirmed, recoveringConfirmed, isRecoveryPhraseValid } = this.state;

    const recoveryPhraseField = form.$('recoveryPhrase');

    const dialogClasses = classnames([
      styles.dialog,
      'verificationDialog',
    ]);

    const storingUnderstandanceCheckboxClasses = classnames([
      styles.checkbox,
      'storingUnderstandance',
    ]);

    const recoveringUnderstandanceCheckboxClasses = classnames([
      styles.checkbox,
      'recoveringUnderstandance'
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
      >

        <div className={styles.verificationContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
          <div className={styles.content}>

            <Autocomplete
              className={styles.recoveryPhrase}
              options={suggestedMnemonics}
              maxSelections={24}
              ref={(autocomplete) => { this.recoveryPhraseAutocomplete = autocomplete; }}
              {...recoveryPhraseField.bind()}
              error={recoveryPhraseField.error}
              maxVisibleOptions={5}
              noResultsMessage={intl.formatMessage(messages.recoveryPhraseNoResults)}
              skin={<SimpleAutocompleteSkin />}
            />

            <Checkbox
              className={storingUnderstandanceCheckboxClasses}
              label={intl.formatMessage(messages.storingUnderstandanceLabel)}
              onChange={this.onStoringConfirmationChange.bind(this)}
              checked={storingConfirmed}
              disabled={!isRecoveryPhraseValid}
              skin={<SimpleCheckboxSkin />}
            />

            <Checkbox
              className={recoveringUnderstandanceCheckboxClasses}
              label={intl.formatMessage(messages.recoveringUnderstandanceLabel)}
              onChange={this.onRecoveringConfirmationChange.bind(this)}
              checked={recoveringConfirmed}
              disabled={!isRecoveryPhraseValid}
              skin={<SimpleCheckboxSkin />}
            />
          </div>
        </div>
        {error ?
          <p className={styles.error}>
            <FormattedHTMLMessage {...messages.errorMessage} />
          </p> : null
        }
      </Dialog>
    );
  }

  onStoringConfirmationChange = () => {
    this.setState({
      storingConfirmed: !this.state.storingConfirmed,
    });
  };

  onRecoveringConfirmationChange = () => {
    this.setState({
      recoveringConfirmed: !this.state.recoveringConfirmed,
    });
  };
}
