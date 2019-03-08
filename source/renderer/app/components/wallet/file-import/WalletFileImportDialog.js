// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './WalletFileImportDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  headline: {
    id: 'wallet.file.import.dialog.headline',
    defaultMessage: '!!!Import Wallet',
    description: 'headline for "Import wallet from file" dialog.'
  },
  walletFileLabel: {
    id: 'wallet.file.import.dialog.walletFileLabel',
    defaultMessage: '!!!Import file',
    description: 'Label "Import file" on the dialog for importing a wallet from a file.'
  },
  walletFileHint: {
    id: 'wallet.file.import.dialog.walletFileHint',
    defaultMessage: '!!!Drop file here or click to choose',
    description: 'Hint for the file upload field on the dialog for importing a wallet from a file.'
  },
  submitLabel: {
    id: 'wallet.file.import.dialog.submitLabel',
    defaultMessage: '!!!Import wallet',
    description: 'Label "Import wallet" submit button on the dialog for importing a wallet from a file.'
  },
  spendingPasswordLabel: {
    id: 'wallet.file.import.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for the "Spending password" input in the wallet file import dialog.',
  },
  spendingPasswordPlaceholder: {
    id: 'wallet.file.import.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Enter Spending password to unlock the file',
    description: 'Placeholder for the "Spending password" input in the wallet file import dialog.',
  },
});

type Props = {
  onSubmit: Function,
  onClose: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
};

type State = {
  importedFileHasPassword: boolean,
};

@observer
export default class WalletFileImportDialog extends Component<Props, State> {

  state = {
    importedFileHasPassword: false
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      walletFile: {
        label: this.context.intl.formatMessage(messages.walletFileLabel),
        placeholder: this.context.intl.formatMessage(messages.walletFileHint),
        type: 'file',
      },
      spendingPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.spendingPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.spendingPasswordPlaceholder),
        value: '',
        validators: [({ field }) => {
          if (!this.state.importedFileHasPassword) return [true];
          return [
            field.value.length >= 7,
            this.context.intl.formatMessage(globalMessages.invalidSpendingPassword)
          ];
        }],
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { importedFileHasPassword } = this.state;
        const { walletFile, spendingPassword } = form.values();
        const walletData = {
          filePath: walletFile.path,
          spendingPassword: importedFileHasPassword ? spendingPassword : null,
        };
        this.props.onSubmit(walletData);
      },
      onError: () => {}
    });
  };

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { isSubmitting, error, onClose } = this.props;
    const { importedFileHasPassword } = this.state;

    const dialogClasses = classnames([
      styles.component,
      'WalletFileImportDialog',
    ]);

    const walletFile = form.$('walletFile');
    const spendingPasswordField = form.$('spendingPassword');

    const actions = [{
      className: isSubmitting ? styles.isSubmitting : null,
      label: intl.formatMessage(messages.submitLabel),
      primary: true,
      disabled: isSubmitting || !(walletFile.value instanceof File),
      onClick: this.submit,
    }];

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
            {...walletFile.bind()}
            selectedFile={walletFile.value}
            onFileSelected={(file) => {
              this.setState({
                importedFileHasPassword: file.name.indexOf('.key.locked') > -1
              });
              // "set(value)" is an unbound method and thus must be explicitly called
              walletFile.set(file);
            }}
          />
        </div>

        {importedFileHasPassword ? (
          <div className={styles.spendingPassword}>
            <Input
              className="spendingPassword"
              {...spendingPasswordField.bind()}
              error={spendingPasswordField.error}
              skin={InputSkin}
            />
          </div>
        ) : null}

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

      </Dialog>
    );
  }

}
