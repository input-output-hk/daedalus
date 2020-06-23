// @flow
// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
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
import styles from './WalletCreateDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { submitOnEnter } from '../../utils/form';

const messages = defineMessages({
  dialogTitleItn: {
    id: 'wallet.create.dialog.title.itn',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create form.',
  },
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a wallet',
    description: 'Title "Create a wallet" in the wallet create form.',
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
  createPersonalWalletItn: {
    id: 'wallet.create.dialog.create.personal.wallet.button.label.itn',
    defaultMessage: '!!!Create Shelley wallet',
    description:
      'Label for the "Create Shelley wallet" button on create wallet dialog.',
  },
  createPersonalWallet: {
    id: 'wallet.create.dialog.create.personal.wallet.button.label',
    defaultMessage: '!!!Create wallet',
    description:
      'Label for the "Create wallet" button on create wallet dialog.',
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
});

const { isIncentivizedTestnet } = global;

type Props = {
  onSubmit: Function,
  onCancel: Function,
};

type State = {
  isSubmitting: boolean,
};

@observer
export default class WalletCreateDialog extends Component<Props, State> {
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
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        this.setState({ isSubmitting: true });
        const { walletName, spendingPassword } = form.values();
        const walletData = {
          name: walletName,
          spendingPassword,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {
        handleFormErrors('.SimpleFormField_error', { focusElement: true });
        this.setState({ isSubmitting: false });
      },
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onCancel } = this.props;
    const { isSubmitting } = this.state;
    const dialogClasses = classnames([styles.component, 'WalletCreateDialog']);

    const walletNameField = form.$('walletName');
    const spendingPasswordField = form.$('spendingPassword');
    const repeatedPasswordField = form.$('repeatPassword');

    const canSubmit = !isSubmitting && form.isValid;

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        disabled: !canSubmit,
        label: this.context.intl.formatMessage(
          isIncentivizedTestnet
            ? messages.createPersonalWalletItn
            : messages.createPersonalWallet
        ),
        primary: true,
        onClick: this.submit,
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(
          isIncentivizedTestnet ? messages.dialogTitleItn : messages.dialogTitle
        )}
        actions={actions}
        closeOnOverlayClick
        onClose={!isSubmitting ? onCancel : () => {}}
        closeButton={<DialogCloseButton />}
      >
        <Input
          className="walletName"
          onKeyPress={this.handleSubmitOnEnter}
          ref={input => {
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
            <Input
              className="spendingPassword"
              onKeyPress={this.handleSubmitOnEnter}
              {...spendingPasswordField.bind()}
              error={spendingPasswordField.error}
              skin={InputSkin}
            />
            <Input
              className="repeatedPassword"
              onKeyPress={this.handleSubmitOnEnter}
              {...repeatedPasswordField.bind()}
              error={repeatedPasswordField.error}
              skin={InputSkin}
            />
            <p className={styles.passwordInstructions}>
              <FormattedHTMLMessage {...globalMessages.passwordInstructions} />
            </p>
          </div>
        </div>
      </Dialog>
    );
  }
}
