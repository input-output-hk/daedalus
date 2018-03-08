// @flow
import React, { Component } from 'react';
import { join, isEqual } from 'lodash';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Autocomplete from 'react-polymorph/lib/components/Autocomplete';
import SimpleAutocompleteSkin from 'react-polymorph/lib/skins/simple/raw/AutocompleteSkin';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { InvalidMnemonicError } from '../../../i18n/errors';
import globalMessages from '../../../i18n/global-messages';
import showPasswordIcon from '../../../assets/images/show-pass-ic.inline.svg';
import hidePasswordIcon from '../../../assets/images/hide-pass-ic.inline.svg';

import styles from './VerificationDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.verification.dialog.headline',
    defaultMessage: '!!!Verify certificate',
    description: 'Headline for the "Paper wallet create certificate verification dialog".'
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.verification.dialog.subtitle',
    defaultMessage: '!!!Enter your shielded recovery phrase and your password to verify your paper wallet certificate.',
    description: '"Paper wallet create certificate verification dialog" subtitle.'
  },
  recoveryPhraseLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.recoveryPhrase.label',
    defaultMessage: '!!!Shielded recovery phrase',
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
  passwordLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.password.label',
    defaultMessage: '!!!Password',
    description: '"Paper wallet create certificate verification dialog" password label.'
  },
  passwordHint: {
    id: 'paper.wallet.create.certificate.verification.dialog.password.hint',
    defaultMessage: '!!!Type password',
    description: '"Paper wallet create certificate verification dialog" password hint.'
  },
  clearButtonLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.button.clearLabel',
    defaultMessage: '!!!Clear',
    description: '"Paper wallet create certificate verification dialog" button clear label.'
  },
  storingUnderstandanceLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.storingUnderstandanceConfirmationLabel',
    defaultMessage: '!!!I understand that the created wallet will not be stored in Daedalus after this step.',
    description: '"Paper wallet create certificate verification dialog" storing understandance confirmation.'
  },
  recoveringUnderstandanceLabel: {
    id: 'paper.wallet.create.certificate.verification.dialog.recoveringUnderstandanceConfirmationLabel',
    defaultMessage: '!!!I understand that my wallet can only be recovered using my paper wallet certificate and the password I have chosen.',
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
  showPassword: boolean,
  isPasswordValid: boolean,
  isRecoveryPhraseValid: boolean,
};

type Props = {
  walletCertificatePassword: string,
  walletCertificateRecoveryPhrase: string,
  error: boolean,
  suggestedMnemonics: Array<string>,
  onContinue: Function,
};

@observer
// eslint-disable-next-line
export default class VerificationDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    storingConfirmed: false,
    recoveringConfirmed: false,
    showPassword: false,
    isPasswordValid: false,
    isRecoveryPhraseValid: false,
  }

  form = new ReactToolboxMobxForm({
    fields: {
      recoveryPhrase: {
        label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
        placeholder: this.context.intl.formatMessage(messages.recoveryPhraseHint),
        value: '',
        validators: [({ field }) => {
          const { walletCertificateRecoveryPhrase } = this.props;
          const {
            storingConfirmed,
            recoveringConfirmed,
          } = this.state;

          const value = join(field.value, ' ');
          if (value === '') return [false, this.context.intl.formatMessage(globalMessages.fieldIsRequired)];
          const isRecoveryPhraseValid = isEqual(walletCertificateRecoveryPhrase, field.value);
          this.setState({
            isRecoveryPhraseValid,
            // uncheck confirmation boxes if recovery phrase is not valid and mark as disabled
            storingConfirmed: isRecoveryPhraseValid ? storingConfirmed : false,
            recoveringConfirmed: isRecoveryPhraseValid ? recoveringConfirmed : false,
          });
          return [
            isRecoveryPhraseValid,
            this.context.intl.formatMessage(new InvalidMnemonicError())
          ];

        }],
      },
      password: {
        label: this.context.intl.formatMessage(messages.passwordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordHint),
        value: '',
        validators: [({ field }) => {
          const {
            storingConfirmed,
            recoveringConfirmed,
          } = this.state;

          if (field.value === '') return [false, this.context.intl.formatMessage(globalMessages.fieldIsRequired)];
          const isPasswordValid = this.props.walletCertificatePassword === field.value;
          this.setState({
            isPasswordValid,
            // uncheck confirmation boxes if password is not valid and mark as disabled
            storingConfirmed: isPasswordValid ? storingConfirmed : false,
            recoveringConfirmed: isPasswordValid ? recoveringConfirmed : false,
          });
          return ([
            isPasswordValid,
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ]);
        }]
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
        const { recoveryPhrase, password } = form.values();
        const data = {
          recoveryPhrase,
          password,
        };

        this.props.onContinue(data);
      },
      onError: () => {},
    });
  };

  resetForm = () => {
    const { form } = this;
    form.$('password').reset();
    form.$('recoveryPhrase').reset();
    this.setState({
      storingConfirmed: false,
      recoveringConfirmed: false,
    });
  }

  render() {
    const { intl } = this.context;
    const { form, resetForm } = this;
    const { suggestedMnemonics, error } = this.props;
    const {
      showPassword,
      storingConfirmed,
      recoveringConfirmed,
      isPasswordValid,
      isRecoveryPhraseValid,
    } = this.state;

    console.debug('state: ', this.state);

    const passwordField = form.$('password');
    const recoveryPhraseField = form.$('recoveryPhrase');

    const dialogClasses = classnames([
      styles.dialog,
      'verificationDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.clearButtonLabel),
        onClick: resetForm.bind(this),
      },
      {
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
              maxSelections={15}
              {...recoveryPhraseField.bind()}
              error={recoveryPhraseField.error}
              maxVisibleOptions={5}
              noResultsMessage={intl.formatMessage(messages.recoveryPhraseNoResults)}
              skin={<SimpleAutocompleteSkin />}
            />

            <div className={styles.password}>
              <Input
                className="password"
                {...passwordField.bind()}
                type={showPassword ? 'text' : 'password'}
                error={passwordField.error}
                skin={<SimpleInputSkin />}
              />
              <SvgInline
                svg={showPassword ? hidePasswordIcon : showPasswordIcon}
                className={styles.passwordVisibilityToggler}
                onClick={this.onTogglePasswordVisibility.bind(this)}
              />
            </div>

            <Checkbox
              className={styles.checkbox}
              label={intl.formatMessage(messages.storingUnderstandanceLabel)}
              onChange={this.onStoringConfirmationChange.bind(this)}
              checked={storingConfirmed}
              disabled={!isPasswordValid || !isRecoveryPhraseValid}
              skin={<SimpleCheckboxSkin />}
            />

            <Checkbox
              className={styles.checkbox}
              label={intl.formatMessage(messages.recoveringUnderstandanceLabel)}
              onChange={this.onRecoveringConfirmationChange.bind(this)}
              checked={recoveringConfirmed}
              disabled={!isPasswordValid || !isRecoveryPhraseValid}
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
      storingConfirmed: !this.state.storingConfirmed
    });
  };

  onTogglePasswordVisibility = () => {
    this.setState({ showPassword: !this.state.showPassword });
  };

  onRecoveringConfirmationChange = () => {
    this.setState({
      recoveringConfirmed: !this.state.recoveringConfirmed
    });
  };
}
