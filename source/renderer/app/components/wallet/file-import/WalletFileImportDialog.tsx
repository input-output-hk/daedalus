import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
// import { Input } from 'react-polymorph/lib/components/Input';
// import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import {
  isValidWalletName, // isValidSpendingPassword,
  // isValidRepeatPassword,
} from '../../../utils/validations';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletFileImportDialog.scss'... Remove this comment to see the full error message
import styles from './WalletFileImportDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  headline: {
    id: 'wallet.file.import.dialog.headline',
    defaultMessage: '!!!Import Wallet',
    description: 'headline for "Import wallet from file" dialog.',
  },
  walletFileLabel: {
    id: 'wallet.file.import.dialog.walletFileLabel',
    defaultMessage: '!!!Import file',
    description:
      'Label "Import file" on the dialog for importing a wallet from a file.',
  },
  walletFileHint: {
    id: 'wallet.file.import.dialog.walletFileHint',
    defaultMessage: '!!!Drop file here or click to choose',
    description:
      'Hint for the file upload field on the dialog for importing a wallet from a file.',
  },
  walletNameInputLabel: {
    id: 'wallet.file.import.dialog.wallet.name.input.label',
    defaultMessage: '!!!Wallet name',
    description:
      'Label for the "wallet name" input in the wallet file import dialog.',
  },
  walletNameInputHint: {
    id: 'wallet.file.import.dialog.wallet.name.input.hint',
    defaultMessage: '!!!e.g: Shopping Wallet',
    description: 'Hint for the "Wallet name" in the wallet file import dialog.',
  },
  submitLabel: {
    id: 'wallet.file.import.dialog.submitLabel',
    defaultMessage: '!!!Import wallet',
    description:
      'Label "Import wallet" submit button on the dialog for importing a wallet from a file.',
  },
  spendingPasswordLabel: {
    id: 'wallet.file.import.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Wallet password',
    description:
      'Label for the "Wallet password" input in the wallet file import dialog.',
  },
  repeatPasswordLabel: {
    id: 'wallet.file.import.dialog.repeatPasswordLabel',
    defaultMessage: '!!!Repeat password',
    description:
      'Label for the "Repeat password" input in the wallet file import dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.file.import.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description:
      'Placeholder for the "Password" inputs in the wallet file import dialog.',
  },
});
type Props = {
  onSubmit: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  isSubmitting: boolean;
  error: LocalizableError | null | undefined;
};

@observer
class WalletFileImportDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        walletFilePath: {
          label: 'filePath',
          placeholder: 'filePath',
          type: 'hidden',
        },
        walletName: {
          label: this.context.intl.formatMessage(messages.walletNameInputLabel),
          placeholder: this.context.intl.formatMessage(
            messages.walletNameInputHint
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (field.value.length === 0) return [true];
              return [
                isValidWalletName(field.value),
                this.context.intl.formatMessage(
                  globalMessages.invalidWalletName
                ),
              ];
            },
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
            () => {
              // const repeatPasswordField = form.$('repeatPassword');
              // if (repeatPasswordField.value.length > 0) {
              //   repeatPasswordField.validate({ showErrors: true });
              // }
              // return [
              //   isValidSpendingPassword(field.value),
              //   this.context.intl.formatMessage(
              //     globalMessages.invalidSpendingPassword
              //   ),
              // ];
              return [true]; // @API TODO - missing API v2 endpoint and password declaration
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
            () => {
              // const spendingPassword = form.$('spendingPassword').value;
              // if (spendingPassword.length === 0) return [true];
              // return [
              //   isValidRepeatPassword(spendingPassword, field.value),
              //   this.context.intl.formatMessage(
              //     globalMessages.invalidRepeatPassword
              //   ),
              // ];
              return [true]; // @API TODO - missing API v2 endpoint and password declaration
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
        const { walletFilePath, spendingPassword, walletName } = form.values();
        const walletData = {
          filePath: walletFilePath,
          spendingPassword,
          walletName: walletName.length > 0 ? walletName : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {},
    });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { isSubmitting, error, onClose } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const walletFilePath = form.$('walletFilePath');
    const dialogClasses = classnames([
      styles.component,
      'WalletFileImportDialog',
    ]);
    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: intl.formatMessage(messages.submitLabel),
        primary: true,
        disabled: isSubmitting || !walletFilePath.value,
        onClick: this.submit,
      },
    ];
    // const walletNameField = form.$('walletName');
    // const spendingPasswordField = form.$('spendingPassword');
    // const repeatedPasswordField = form.$('repeatPassword');
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.fileUpload}>
          <FileUploadWidget
            label={intl.formatMessage(messages.walletFileLabel)}
            placeholder={intl.formatMessage(messages.walletFileHint)}
            acceptedFileTypes={['*']}
            selectedFile={walletFilePath.value}
            onFileSelected={(filePath) => {
              // "set(value)" is an unbound method and thus must be explicitly called
              walletFilePath.set(filePath);
            }}
          />
        </div>

        {/* TODO: re-enable when wallet-name
           support is added to the API endpoint
          <Input
           className="walletName"
           {...walletNameField.bind()}
           error={walletNameField.error}
           skin={InputSkin}
         />
          <div className={styles.spendingPassword}>
           <div className={styles.spendingPasswordFields}>
             <Input
               className="spendingPassword"
               {...spendingPasswordField.bind()}
               error={spendingPasswordField.error}
               skin={InputSkin}
             />
             <Input
               className="repeatedPassword"
               {...repeatedPasswordField.bind()}
               error={repeatedPasswordField.error}
               skin={InputSkin}
             />
             <p className={styles.passwordInstructions}>
               {intl.formatMessage(globalMessages.passwordInstructions)}
             </p>
           </div>
         </div>
        */}

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}
      </Dialog>
    );
  }
}

export default WalletFileImportDialog;
