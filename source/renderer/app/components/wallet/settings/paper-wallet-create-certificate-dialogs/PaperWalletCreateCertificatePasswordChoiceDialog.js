// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/raw/CheckboxSkin';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import ReactToolboxMobxForm from '../../../../utils/ReactToolboxMobxForm';
import { isValidWalletPassword, isValidRepeatPassword } from '../../../../utils/validations';
import globalMessages from '../../../../i18n/global-messages';
import styles from './PaperWalletCreateCertificatePasswordChoiceDialog.scss';

import showPasswordIcon from '../../../../assets/images/show-pass-ic.inline.svg';
import hidePasswordIcon from '../../../../assets/images/hide-pass-ic.inline.svg';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.headline',
    defaultMessage: '!!!Choose your password',
    description: 'Headline for the "Paper wallet create certificate password choice dialog".'
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.subtitle',
    defaultMessage: '!!!A password you chose will be used to shield your recovery prase on the certificate. You will not be able to restore your wallet without this password.',
    description: '"Paper wallet create certificate password choice dialog" subtitle.'
  },
  passwordComplexityText: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.password.ComplexityText',
    defaultMessage: '!!!Note that password needs to be at least <strong>7 characters long</strong>, and have at least <strong>1 uppercase character</strong>, <strong>1 lowercase character</strong> and <strong>1 number</strong>.',
    description: '"Paper wallet create certificate password choice dialog" password complexity text.'
  },
  passwordLabel: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.password.label',
    defaultMessage: '!!!Enter password',
    description: '"Paper wallet create certificate password choice dialog" password label.'
  },
  passwordHint: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.password.hint',
    defaultMessage: '!!!Type password',
    description: '"Paper wallet create certificate password choice dialog" password hint.'
  },
  repeatPasswordLabel: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.repeatPassword.label',
    defaultMessage: '!!!Repeat password',
    description: '"Paper wallet create certificate password choice dialog" repeat password label.'
  },
  repeatPasswordHint: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.repeatPassword.hint',
    defaultMessage: '!!!Repeat password',
    description: '"Paper wallet create certificate password choice dialog" repeat password hint.'
  },
  keepPasswordSecuredConfirmationLabel: {
    id: 'paper.wallet.create.certificate.passwordChoice.dialog.password.keepConfirmation',
    defaultMessage: '!!!I understand the importance of the password and I will keep it secure.',
    description: '"Paper wallet create certificate password choice dialog" password keep confirmation.'
  },
});

type State = {
  keepPasswordSecuredConfirmation: boolean,
  showPassword: boolean,
  showRepeatPassword: boolean,
};

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
};

@observer
// eslint-disable-next-line
export default class PaperWalletCreateCertificatePasswordChoiceDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    keepPasswordSecuredConfirmation: false,
    showPassword: false,
    showRepeatPassword: false,
  }

  form = new ReactToolboxMobxForm({
    fields: {
      password: {
        label: this.context.intl.formatMessage(messages.passwordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordHint),
        value: '',
        validators: [({ field, form }) => {
          const repeatPasswordField = form.$('repeatPassword');
          if (repeatPasswordField.value.length > 0) repeatPasswordField.validate(form);
          return [
            isValidWalletPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletPassword)
          ];
        }]
      },
      repeatPassword: {
        label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.repeatPasswordHint),
        value: '',
        validators: [({ field, form }) => {
          const walletPassword = form.$('password').value;
          return [
            isValidRepeatPassword(walletPassword, field.value),
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
        const { password, repeatPassword } = form.values();
        const data = {
          password, repeatPassword
        };

        this.props.onContinue(data);
      },
      onError: () => {},
    });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { onClose, onBack } = this.props;
    const {
      keepPasswordSecuredConfirmation, showPassword, showRepeatPassword
    } = this.state;

    const passwordField = form.$('password');
    const repeatPasswordField = form.$('repeatPassword');

    const dialogClasses = classnames([
      styles.component,
      'PaperWalletCreateCertificatePasswordChoiceDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        disabled: !keepPasswordSecuredConfirmation,
        onClick: this.submit.bind(this),
      }
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

        <div className={styles.passwordChoiceContentWrapper}>
          <p className={styles.subtitle}>{intl.formatMessage(messages.subtitle)}</p>
          <div className={styles.content}>

            <p className={styles.passwordComplexityText}>
              <FormattedHTMLMessage {...messages.passwordComplexityText} />
            </p>

            <div className={styles.setPasswordWrapper}>

              <div className={styles.passwordWrapper}>
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

              <div className={styles.repeatPasswordWrapper}>
                <Input
                  className="repeatPassword"
                  {...repeatPasswordField.bind()}
                  type={showRepeatPassword ? 'text' : 'password'}
                  error={repeatPasswordField.error}
                  skin={<SimpleInputSkin />}
                />
                <SvgInline
                  svg={showRepeatPassword ? hidePasswordIcon : showPasswordIcon}
                  className={styles.passwordVisibilityToggler}
                  onClick={this.onShowRepeatPassword.bind(this)}
                />
              </div>

            </div>

            <Checkbox
              className={styles.checkbox}
              label={intl.formatMessage(messages.keepPasswordSecuredConfirmationLabel)}
              onChange={this.onConfirmationValueChange.bind(this)}
              checked={keepPasswordSecuredConfirmation}
              skin={<SimpleCheckboxSkin />}
            />
          </div>
        </div>

      </Dialog>
    );
  }

  onConfirmationValueChange = () => {
    this.setState({ keepPasswordSecuredConfirmation: !this.state.keepPasswordSecuredConfirmation });
  };

  onTogglePasswordVisibility = () => {
    this.setState({ showPassword: !this.state.showPassword });
  };

  onShowRepeatPassword = () => {
    this.setState({ showRepeatPassword: !this.state.showRepeatPassword });
  };

}
