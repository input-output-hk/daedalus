// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Dialog from 'react-toolbox/lib/dialog/Dialog';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import FileUploadWidget from '../../widgets/forms/FileUploadWidget';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './WalletKeyImportDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.key.import.dialog.headline',
    defaultMessage: '!!!Import wallet from file with the key',
    description: 'headline for "Import wallet from file with the key" dialog.'
  },
  keyFileLabel: {
    id: 'wallet.key.import.dialog.keyFileLabel',
    defaultMessage: '!!!Upload your key',
    description: 'Label "Upload your key" on the dialog for importing a wallet from the key.'
  },
  keyFileHint: {
    id: 'wallet.key.import.dialog.keyFileHint',
    defaultMessage: '!!!Drop file here or click to choose',
    description: 'Hint for the key file upload on the dialog for importing a wallet from the key.'
  },
  submitLabel: {
    id: 'wallet.key.import.dialog.submitLabel',
    defaultMessage: '!!!Import wallet',
    description: 'Label "Import you key" submit button on the dialog for importing a wallet from the key.'
  },
});

@observer
export default class WalletKeyImportDialog extends Component {

  static propTypes = {
    onSubmit: PropTypes.func.isRequired,
    onClose: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { keyFile } = form.values();
        this.props.onSubmit({ filePath: keyFile.path });
      },
      onError: () => {}
    });
  };

  form = new ReactToolboxMobxForm({
    fields: {
      keyFile: {
        label: this.context.intl.formatMessage(messages.keyFileLabel),
        placeholder: this.context.intl.formatMessage(messages.keyFileHint),
        type: 'file',
        bindings: 'ReactToolbox',
      }
    }
  }, {
    options: {
      validateOnChange: false,
    }
  });

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { isSubmitting, error, onClose } = this.props;
    const keyFile = form.$('keyFile');
    const dialogClasses = classnames([
      styles.component,
      'WalletKeyImportDialog',
      isSubmitting ? styles.isSubmitting : null
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.submitLabel),
        primary: true,
        disabled: !(keyFile.value instanceof File),
        onClick: () => this.submit()
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        onOverlayClick={onClose}
        active
      >

        <div className={styles.keyUpload}>
          <FileUploadWidget
            {...keyFile.bind()}
            selectedFile={keyFile.value}
            onFileSelected={keyFile.onChange}
          />
        </div>

        {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        <DialogCloseButton onClose={onClose} />

      </Dialog>
    );
  }

}
