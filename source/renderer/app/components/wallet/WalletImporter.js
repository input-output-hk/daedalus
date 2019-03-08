// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { TextArea } from 'react-polymorph/lib/components/TextArea';
import { TextAreaSkin } from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import BorderedBox from '../widgets/BorderedBox';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import FileUploadWidget from '../widgets/forms/FileUploadWidget';
import styles from './WalletImporter.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';

export const messages = defineMessages({
  headline: {
    id: 'wallet.importer.headline',
    defaultMessage: '!!!Wallet importer',
    description: 'Headline of the wallet importer page.',
  },
  instructions: {
    id: 'wallet.importer.instructions',
    defaultMessage: '!!!In order to import wallets select a folder which contains wallet key files.',
    description: 'Detailed instructions for importing wallets.',
  },
  keyFileLabel: {
    id: 'wallet.importer.keyFileLabel',
    defaultMessage: '!!!Secrets key file',
    description: 'Label "Secrets key file" on the wallet importer page.'
  },
  keyFileHint: {
    id: 'wallet.importer.keyFileHint',
    defaultMessage: '!!!Drop your "secret.key" file here or click to choose',
    description: 'Hint for the secrets key file upload field on the wallet importer page.'
  },
  passwordsListLabel: {
    id: 'wallet.importer.passwordsListLabel',
    defaultMessage: '!!!Passwords list',
    description: 'Label "Passwords list" on the wallet importer page.'
  },
  passwordsListHint: {
    id: 'wallet.importer.passwordsListHint',
    defaultMessage: '!!!Enter your potential wallet passwords line by line',
    description: 'Hint for the passwords list field on the wallet importer page.'
  },
});

type Props = {};

@observer
export default class WalletImporter extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      keyFile: {
        label: this.context.intl.formatMessage(messages.keyFileLabel),
        placeholder: this.context.intl.formatMessage(messages.keyFileHint),
        type: 'file',
      },
      passwords: {
        label: this.context.intl.formatMessage(messages.passwordsListLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordsListHint),
        value: '',
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  render() {
    const { intl } = this.context;
    const { form } = this;
    const keyFileField = form.$('keyFile');
    const passwordsField = form.$('passwords');

    return (
      <div className={styles.component}>

        <BorderedBox>

          <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

          <div className={styles.instructions}>
            <p>{intl.formatMessage(messages.instructions)}</p>
          </div>

          <div className={styles.fileUpload}>
            <FileUploadWidget
              {...keyFileField.bind()}
              selectedFile={keyFileField.value}
              acceptedFileTypes=".key"
              onFileSelected={(file) => {
                // "set(value)" is an unbound method and thus must be explicitly called
                keyFileField.set(file);
              }}
            />
          </div>

          {keyFileField.value ? (
            <div>
              <TextArea
                {...passwordsField.bind()}
                className="passwordsList"
                autoResize={false}
                error={passwordsField.error}
                rows={5}
                skin={TextAreaSkin}
              />
            </div>
          ) : (
            <div>PLEASE SELECT A KEY FILE TO START</div>
          )}

        </BorderedBox>

      </div>
    );
  }

}
