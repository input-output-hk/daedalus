// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Input } from 'react-polymorph/lib/components/Input';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import DialogCloseButton from '../widgets/DialogCloseButton';
import Dialog from '../widgets/Dialog';
import { isValidWalletName, isValidSpendingPassword, isValidRepeatPassword } from '../../utils/validations';
import globalMessages from '../../i18n/global-messages';
import styles from './WalletCreateDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { submitOnEnter } from '../../utils/form';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.'
  },
  walletName: {
    id: 'wallet.create.dialog.name.label',
    defaultMessage: '!!!Wallet Name',
    description: 'Label for the "Wallet Name" text input in the wallet create form.'
  },
  walletNameHint: {
    id: 'wallet.create.dialog.walletNameHint',
    defaultMessage: '!!!e.g: Shopping Wallet',
    description: 'Hint for the "Wallet Name" text input in the wallet create form.'
  },
  createPersonalWallet: {
    id: 'wallet.create.dialog.create.personal.wallet.button.label',
    defaultMessage: '!!!Create personal wallet',
    description: 'Label for the "Create personal wallet" button on create wallet dialog.'
  },
  passwordSwitchPlaceholder: {
    id: 'wallet.create.dialog.passwordSwitchPlaceholder',
    defaultMessage: '!!!Keep your private keys safely encrypted by setting the spending password',
    description: 'Text for the "Activate to create password" switch in the create wallet dialog.',
  },
  passwordSwitchLabel: {
    id: 'wallet.create.dialog.passwordSwitchLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for the "Activate to create password" switch in the create wallet dialog.',
  },
  spendingPasswordLabel: {
    id: 'wallet.create.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description: 'Label for the "Wallet password" input in the create wallet dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.create.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description: 'Label for the "Repeat password" input in the create wallet dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.create.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the create wallet dialog.',
  },
});

type Props = {
  onSubmit: Function,
  onCancel: Function,
};

type State = {
  isSubmitting: boolean,
  createPassword: boolean,
};

@observer
export default class WalletCreateDialog extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isSubmitting: false,
    createPassword: true,
  };

  componentDidMount() {
    setTimeout(() => { this.walletNameInput.focus(); });
  }

  walletNameInput: Input;

  form = new ReactToolboxMobxForm({
    fields: {
      walletName: {
        label: this.context.intl.formatMessage(messages.walletName),
        placeholder: this.context.intl.formatMessage(messages.walletNameHint),
        value: '',
        validators: [({ field }) => (
          [
            isValidWalletName(field.value),
            this.context.intl.formatMessage(globalMessages.invalidWalletName)
          ]
        )],
      },
      spendingPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.spendingPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
          const repeatPasswordField = form.$('repeatPassword');
          if (repeatPasswordField.value.length > 0) {
            repeatPasswordField.validate({ showErrors: true });
          }
          return [
            isValidSpendingPassword(field.value),
            this.context.intl.formatMessage(globalMessages.invalidSpendingPassword)
          ];
        }],
      },
      repeatPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.repeatPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
        validators: [({ field, form }) => {
          if (!this.state.createPassword) return [true];
          const spendingPassword = form.$('spendingPassword').value;
          if (spendingPassword.length === 0) return [true];
          return [
            isValidRepeatPassword(spendingPassword, field.value),
            this.context.intl.formatMessage(globalMessages.invalidRepeatPassword)
          ];
        }],
      },
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        this.setState({ isSubmitting: true });
        const { createPassword } = this.state;
        const { walletName, spendingPassword } = form.values();
        const walletData = {
          name: walletName,
          spendingPassword: createPassword ? spendingPassword : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {
        this.setState({ isSubmitting: false });
      },
    });
  };

  handlePasswordSwitchToggle = (value: boolean) => {
    this.setState({ createPassword: value });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onCancel } = this.props;
    const { createPassword, isSubmitting } = this.state;
    const dialogClasses = classnames([
      styles.component,
      'WalletCreateDialog',
    ]);
    const spendingPasswordFieldsClasses = classnames([
      styles.spendingPasswordFields,
      createPassword ? styles.show : null,
    ]);

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: this.context.intl.formatMessage(messages.createPersonalWallet),
        primary: true,
        onClick: this.submit,
      },
    ];

    const walletNameField = form.$('walletName');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');

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
          onKeyPress={submitOnEnter.bind(this, this.submit)}
          ref={(input) => { this.walletNameInput = input; }}
          {...walletNameField.bind()}
          error={walletNameField.error}
          skin={InputSkin}
        />

        <div className={styles.spendingPassword}>
          <div className={styles.spendingPasswordSwitch}>
            <div className={styles.passwordLabel}>
              {intl.formatMessage(messages.passwordSwitchLabel)}
            </div>
            <Checkbox
              themeId={IDENTIFIERS.SWITCH}
              onChange={this.handlePasswordSwitchToggle}
              label={intl.formatMessage(messages.passwordSwitchPlaceholder)}
              checked={createPassword}
              skin={SwitchSkin}
            />
          </div>

          <div className={spendingPasswordFieldsClasses}>
            <Input
              className="spendingPassword"
              onKeyPress={submitOnEnter.bind(this, this.submit)}
              {...spendingPasswordField.bind()}
              error={spendingPasswordField.error}
              skin={InputSkin}
            />
            <Input
              className="repeatedPassword"
              onKeyPress={submitOnEnter.bind(this, this.submit)}
              {...repeatedPasswordField.bind()}
              error={repeatedPasswordField.error}
              skin={InputSkin}
            />
            <p className={styles.passwordInstructions}>
              {intl.formatMessage(globalMessages.passwordInstructions)}
            </p>
          </div>
        </div>

      </Dialog>
    );
  }

}
