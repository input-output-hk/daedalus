// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import ReactToolboxMobxForm, {
  handleFormErrors,
} from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import {
  isValidWalletName,
  isValidSpendingPassword,
  isValidRepeatPassword,
} from '../../utils/validations';
import globalMessages from '../../i18n/global-messages';
import { PasswordInput } from '../widgets/forms/PasswordInput';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletCreateDialog.scss' or ... Remove this comment to see the full error message
import styles from './WalletCreateDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { submitOnEnter } from '../../utils/form';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/info-icon.... Remove this comment to see the full error message
import infoIconInline from '../../assets/images/info-icon.inline.svg';
import LoadingSpinner from '../widgets/LoadingSpinner';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
  walletName: {
    id: 'wallet.create.dialog.name.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the "Wallet Name" text input in the wallet create form.',
  },
  walletNameHint: {
    id: 'wallet.create.dialog.walletNameHint',
    defaultMessage: '!!!Enter wallet name',
    description:
      'Hint for the "Wallet Name" text input in the wallet create form.',
  },
  createPersonalWallet: {
    id: 'wallet.create.dialog.create.personal.wallet.button.label',
    defaultMessage: '!!!Create Shelley wallet',
    description:
      'Label for the "Create Shelley wallet" button on create wallet dialog.',
  },
  passwordSectionLabel: {
    id: 'wallet.create.dialog.passwordSectionLabel',
    defaultMessage: '!!!Spending password',
    description: 'Password creation label.',
  },
  passwordSectionDescription: {
    id: 'wallet.create.dialog.passwordSectionDescription',
    defaultMessage: '!!!Keep your wallet secure by setting a spending password',
    description: 'Password creation description.',
  },
  spendingPasswordLabel: {
    id: 'wallet.create.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description:
      'Label for the "Wallet password" input in the create wallet dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.create.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the create wallet dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.create.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the create wallet dialog.',
  },
  passwordTooltip: {
    id: 'wallet.dialog.passwordTooltip',
    defaultMessage:
      'We recommend using a password manager app to manage and store your spending password. Generate a unique password using a password manager and paste it here. Passwords should never be reused.',
    description: 'Tooltip for the password input in the wallet dialog.',
  },
});
type Props = {
  onSubmit: (...args: Array<any>) => any;
  onCancel: (...args: Array<any>) => any;
  currentLocale: string;
};
type State = {
  isSubmitting: boolean;
};

@observer
class WalletCreateDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isSubmitting: false,
  };

  componentDidMount() {
    setTimeout(() => {
      this.walletNameInput.focus();
    });
  }

  walletNameInput: Input;
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        walletName: {
          label: this.context.intl.formatMessage(messages.walletName),
          placeholder: this.context.intl.formatMessage(messages.walletNameHint),
          value: '',
          validators: [
            ({ field }) => [
              isValidWalletName(field.value),
              this.context.intl.formatMessage(globalMessages.invalidWalletName),
            ],
          ],
        },
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const repeatPasswordField = form.$('repeatPassword');
              const isRepeatPasswordFieldSet =
                repeatPasswordField.value.length > 0;
              repeatPasswordField.validate({
                showErrors: isRepeatPasswordFieldSet,
              });
              return [
                isValidSpendingPassword(field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidSpendingPassword
                ),
              ];
            },
          ],
        },
        repeatPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passwordFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field, form }) => {
              const spendingPassword = form.$('spendingPassword').value;
              return [
                isValidRepeatPassword(spendingPassword, field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidRepeatPassword
                ),
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
    this.setState({
      isSubmitting: false,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        this.setState({
          isSubmitting: true,
        });
        const { walletName, spendingPassword } = form.values();
        const walletData = {
          name: walletName,
          spendingPassword,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {
        handleFormErrors('.SimpleFormField_error', {
          focusElement: true,
        });
        this.setState({
          isSubmitting: false,
        });
      },
    });
  };
  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onCancel, currentLocale } = this.props;
    const { isSubmitting } = this.state;
    const dialogClasses = classnames([styles.component, 'WalletCreateDialog']);
    const spendingPasswordClasses = classnames([
      styles.spendingPasswordField,
      currentLocale === 'ja-JP' ? styles.jpLangTooltipIcon : '',
    ]);
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const walletNameField = form.$('walletName');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const spendingPasswordField = form.$('spendingPassword');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const repeatedPasswordField = form.$('repeatPassword');
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
    const canSubmit = !isSubmitting && form.isValid;
    const buttonLabel = !isSubmitting ? (
      this.context.intl.formatMessage(messages.createPersonalWallet)
    ) : (
      <LoadingSpinner />
    );
    const actions = [
      {
        disabled: !canSubmit,
        label: buttonLabel,
        primary: true,
        onClick: this.submit,
      },
    ];
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : () => {}}
        closeButton={<DialogCloseButton />}
      >
        <Input
          className="walletName"
          onKeyPress={this.handleSubmitOnEnter}
          ref={(input) => {
            this.walletNameInput = input;
          }}
          {...walletNameField.bind()}
          error={walletNameField.error}
          skin={InputSkin}
        />

        <div className={styles.spendingPasswordWrapper}>
          <div className={styles.passwordSectionLabel}>
            {intl.formatMessage(messages.passwordSectionLabel)}
          </div>

          <div className={styles.passwordSectionDescription}>
            {intl.formatMessage(messages.passwordSectionDescription)}
          </div>

          <div className={styles.spendingPasswordFields}>
            <div className={spendingPasswordClasses}>
              <PasswordInput
                className="spendingPassword"
                onKeyPress={this.handleSubmitOnEnter}
                {...spendingPasswordField.bind()}
              />
              <PopOver
                content={<FormattedHTMLMessage {...messages.passwordTooltip} />}
                key="tooltip"
              >
                <SVGInline svg={infoIconInline} className={styles.infoIcon} />
              </PopOver>
            </div>
            <div className={styles.spendingPasswordField}>
              <PasswordInput
                className="repeatedPassword"
                onKeyPress={this.handleSubmitOnEnter}
                {...repeatedPasswordField.bind()}
                repeatPassword={spendingPasswordField.value}
                isPasswordRepeat
              />
            </div>
            <p className={styles.passwordInstructions}>
              <FormattedHTMLMessage {...globalMessages.passwordInstructions} />
            </p>
          </div>
        </div>
      </Dialog>
    );
  }
}

export default WalletCreateDialog;
