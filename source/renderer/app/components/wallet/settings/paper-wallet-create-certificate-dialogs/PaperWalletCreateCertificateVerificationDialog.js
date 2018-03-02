// @flow
import React, { Component } from 'react';
import { join } from 'lodash';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { isEmpty } from 'validator';
import Autocomplete from 'react-polymorph/lib/components/Autocomplete';
import SimpleAutocompleteSkin from 'react-polymorph/lib/skins/simple/raw/AutocompleteSkin';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import ReactToolboxMobxForm from '../../../../utils/ReactToolboxMobxForm';
import { isValidWalletPassword } from '../../../../utils/validations';
import { InvalidMnemonicError } from '../../../../i18n/errors';
import globalMessages from '../../../../i18n/global-messages';
import showPasswordIcon from '../../../../assets/images/show-pass-ic.inline.svg';

import styles from './PaperWalletCreateCertificateVerificationDialog.scss';

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
});

type State = {
  storingUnderstandanceConfirmed: boolean,
  recoveringUnderstandanceConfirmed: boolean,
  showPassword: boolean,
};

type Props = {
  suggestedMnemonics: Array<string>,
  onPassPhraseChanged: Function,
  mnemonicValidator: Function,
  onContinue: Function,
  onClear: Function,
  onClose: Function,
  onBack: Function,
};

@observer
export default class PaperWalletCreateCertificateVerificationDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    storingUnderstandanceConfirmed: false,
    recoveringUnderstandanceConfirmed: false,
    showPassword: false,
  }

  form = new ReactToolboxMobxForm({
    fields: {
      recoveryPhrase: {
        label: this.context.intl.formatMessage(messages.recoveryPhraseLabel),
        placeholder: this.context.intl.formatMessage(messages.recoveryPhraseHint),
        value: '',
        validators: [({ field }) => {
          const passPhrase = join(field.value, ' ');
          if (!isEmpty(passPhrase)) this.props.onPassPhraseChanged(passPhrase);
          return [
            this.props.mnemonicValidator(passPhrase),
            this.context.intl.formatMessage(new InvalidMnemonicError())
          ];

        }],
      },
      password: {
        label: this.context.intl.formatMessage(messages.passwordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordHint),
        value: '',
        validators: [({ field, form }) => {
          return [
            isValidWalletPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ];
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

  render() {
    const { intl } = this.context;
    const { form } = this;
    const {
      suggestedMnemonics,
      onClose,
      onBack,
      onContinue,
      onClear,
    } = this.props;
    const {
      showPassword,
      storingUnderstandanceConfirmed,
      recoveringUnderstandanceConfirmed,
    } = this.state;

    const passwordField = form.$('password');
    const recoveryPhraseField = form.$('recoveryPhrase');

    const dialogClasses = classnames([
      styles.component,
      'PaperWalletCreateCertificateVerificationDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.clearButtonLabel),
        onClick: onClear,
      },
      {
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        disabled: !storingUnderstandanceConfirmed || !recoveringUnderstandanceConfirmed,
        onClick: this.submit.bind(this),
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >

        <div className={styles.verificationContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
          <div className={styles.content}>

            <Autocomplete
              label={intl.formatMessage(messages.recoveryPhraseLabel)}
              className={styles.recoveryPhrase}
              options={suggestedMnemonics}
              maxSelections={9}
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
                svg={showPasswordIcon}
                className={styles.passwordVisibilityToggler}
                onClick={this.onTogglePasswordVisibility.bind(this)}
              />
            </div>

            <Checkbox
              className={styles.checkbox}
              label={intl.formatMessage(messages.storingUnderstandanceLabel)}
              onChange={this.onStoringConfirmationChange.bind(this)}
              checked={storingUnderstandanceConfirmed}
              skin={<SimpleCheckboxSkin />}
            />

            <Checkbox
              className={styles.checkbox}
              label={intl.formatMessage(messages.recoveringUnderstandanceLabel)}
              onChange={this.onRecoveringConfirmationChange.bind(this)}
              checked={recoveringUnderstandanceConfirmed}
              skin={<SimpleCheckboxSkin />}
            />
          </div>
        </div>

      </Dialog>
    );
  }

  onStoringConfirmationChange = () => {
    this.setState({ storingUnderstandanceConfirmed: !this.state.storingUnderstandanceConfirmed });
  };

  onTogglePasswordVisibility = () => {
    this.setState({ showPassword: !this.state.showPassword });
  };

  onRecoveringConfirmationChange = () => {
    this.setState({ recoveringUnderstandanceConfirmed: !this.state.recoveringUnderstandanceConfirmed });
  };
}
