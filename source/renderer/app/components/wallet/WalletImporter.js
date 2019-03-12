// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { uniq } from 'lodash';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { TextArea } from 'react-polymorph/lib/components/TextArea';
import { TextAreaSkin } from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import TinySwitch from '../widgets/forms/TinySwitch';
import BorderedBox from '../widgets/BorderedBox';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import FileUploadWidget from '../widgets/forms/FileUploadWidget';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './WalletImporter.scss';
import type {
  ExtractedWallet,
  ExtractedWallets,
} from '../../../../common/types/wallet-importer.types';

const MAXIMUM_PASSWORD_COUNT = 20;

export const messages = defineMessages({
  headline: {
    id: 'wallet.importer.headline',
    defaultMessage: '!!!Wallet importer',
    description: 'Headline of the wallet importer page.',
  },
  instructions: {
    id: 'wallet.importer.instructions',
    defaultMessage: '!!!In order to import wallets select a secrets key file.',
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
  maskPasswordsLabel: {
    id: 'wallet.importer.maskPasswordsLabel',
    defaultMessage: '!!!mask passwords',
    description: 'Label "mask passwords" on the wallet importer page.'
  },
  passwordsListHint: {
    id: 'wallet.importer.passwordsListHint',
    defaultMessage: '!!!Enter up to 20 potential wallet passwords line by line',
    description: 'Hint for the passwords list field on the wallet importer page.'
  },
  submitLabel: {
    id: 'wallet.importer.submitLabel',
    defaultMessage: '!!!Match passwords',
    description: 'Label for the "match passwords" submit button on the wallet importer page.'
  },
  extractingWalletsNotification: {
    id: 'wallet.importer.extractingWalletsNotification',
    defaultMessage: '!!!Extracting wallets',
    description: 'Notification shown during wallet extraction on the wallet importer page.'
  },
  noWalletsFoundNotification: {
    id: 'wallet.importer.noWalletsFoundNotification',
    defaultMessage: '!!!No wallets found in the provided secrets key file.',
    description: 'Notification shown if no wallets are extracted on the wallet importer page.'
  },
  walletFileLabel: {
    id: 'wallet.importer.walletFileLabel',
    defaultMessage: '!!!Wallet file',
    description: 'Label "Wallet file" on the wallet importer page.'
  },
  walletPasswordLabel: {
    id: 'wallet.importer.walletPasswordLabel',
    defaultMessage: '!!!Password',
    description: 'Label "Password" on the wallet importer page.'
  },
  walletBalanceLabel: {
    id: 'wallet.importer.walletBalanceLabel',
    defaultMessage: '!!!Balance',
    description: 'Label "Balance" on the wallet importer page.'
  },
  noPasswordHint: {
    id: 'wallet.importer.noPasswordHint',
    defaultMessage: '!!!No password',
    description: 'Hint for the wallet password field on the wallet importer page.'
  },
  unknownPasswordHint: {
    id: 'wallet.importer.unknownPasswordHint',
    defaultMessage: '!!!unknown password',
    description: 'Hint for the wallet password field on the wallet importer page.'
  },
  unknownBalanceHint: {
    id: 'wallet.importer.unknownBalanceHint',
    defaultMessage: '!!!unknown balance',
    description: 'Hint for the wallet balance field on the wallet importer page.'
  },
  importLabel: {
    id: 'wallet.importer.importLabel',
    defaultMessage: '!!!Import',
    description: 'Label for the wallet "Import" button on the wallet importer page.'
  },
  openWalletLabel: {
    id: 'wallet.importer.openLabel',
    defaultMessage: '!!!Open',
    description: 'Label for the wallet "Open" button on the wallet importer page.'
  },
});

type Props = {
  keyFile: ?File,
  isRestoreActive: boolean,
  restoringWalletId: ?string,
  isMatchingPasswords: boolean,
  isExtractingWallets: boolean,
  hasExtractedWallets: boolean,
  extractedWallets: ExtractedWallets,
  onSecretKeyFileSelect: Function,
  onDownloadKeyFile: Function,
  onImportKeyFile: Function,
  onMatchPasswords: Function,
  onOpenWallet: Function,
};

type State = {
  maskPasswords: boolean,
  unmaskedPasswords: string,
};

@observer
export default class WalletImporter extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    maskPasswords: false,
    unmaskedPasswords: '',
  };

  form = new ReactToolboxMobxForm({
    fields: {
      keyFile: {
        label: this.context.intl.formatMessage(messages.keyFileLabel),
        placeholder: this.context.intl.formatMessage(messages.keyFileHint),
        value: this.props.keyFile,
        type: 'file',
      },
      passwords: {
        label: this.context.intl.formatMessage(messages.passwordsListLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordsListHint),
        value: '',
      },
    },
  });

  submit = () => {
    const { form } = this;
    const passwordsField = form.$('passwords');
    const { maskPasswords, unmaskedPasswords } = this.state;
    const passwordsList = maskPasswords ? unmaskedPasswords : passwordsField.value;
    const passwords = uniq(passwordsList.split('\n'));
    this.props.onMatchPasswords(passwords);
  };

  downloadKeyFile = (fileName: string, wallet: ExtractedWallet) => {
    const filePath = global.dialog.showSaveDialog({ defaultPath: fileName });
    if (filePath) this.props.onDownloadKeyFile(wallet, filePath);
  };

  toggleMaskPasswords = () => {
    if (this.state.maskPasswords) {
      // Passwords are masked - we need to unmask them
      this.unmaskPasswords();
    } else {
      // Passwords are not masked - we need to mask them
      const { form } = this;
      const passwordsField = form.$('passwords');
      const passwordsList = passwordsField.value;
      const passwordsByLine = passwordsList.split('\n');
      const maskedPasswordsByLine = passwordsByLine.map((line) => line.replace(/./g, '•'));
      const maskedPasswordsList = maskedPasswordsByLine.join('\n');
      passwordsField.set(maskedPasswordsList);
      this.setState({
        maskPasswords: true,
        unmaskedPasswords: passwordsList,
      });
    }
  };

  unmaskPasswords = () => {
    if (this.state.maskPasswords) {
      // Passwords are masked - we need to unmask them
      const { form } = this;
      const passwordsField = form.$('passwords');
      passwordsField.set(this.state.unmaskedPasswords);
      this.setState({ maskPasswords: false });
    }
  };

  render() {
    const { intl } = this.context;
    const {
      isRestoreActive,
      restoringWalletId,
      isMatchingPasswords,
      isExtractingWallets,
      hasExtractedWallets,
      extractedWallets: wallets,
      onSecretKeyFileSelect,
      onImportKeyFile,
      onOpenWallet,
    } = this.props;
    const { maskPasswords } = this.state;
    const { form, submit } = this;

    const keyFileField = form.$('keyFile');
    const passwordsField = form.$('passwords');

    const generateWalletList = () => (
      wallets.map((wallet) => {
        const { id, index, password, balance, imported } = wallet;
        const fileName = `wallet-${index}.key${password !== '' ? '.locked' : ''}`;
        const isImporting = id && id === restoringWalletId;
        const importButtonClasses = classnames([
          'primary',
          isImporting ? styles.importButtonSpinning : styles.importButton,
        ]);
        return (
          <div key={index} className={styles.walletRow}>
            <div>
              <Input
                label={index === 1 ? intl.formatMessage(messages.walletFileLabel) : null}
                value={fileName}
                skin={InputSkin}
                readOnly
              />
            </div>
            <Input
              label={index === 1 ? intl.formatMessage(messages.walletPasswordLabel) : null}
              placeholder={
                password === '' ?
                  intl.formatMessage(messages.noPasswordHint) :
                  intl.formatMessage(messages.unknownPasswordHint)
              }
              value={password}
              skin={InputSkin}
              type={maskPasswords ? 'password' : 'text'}
              readOnly
            />
            <Input
              label={index === 1 ? intl.formatMessage(messages.walletBalanceLabel) : null}
              placeholder={intl.formatMessage(messages.unknownBalanceHint)}
              value={balance}
              skin={InputSkin}
              readOnly
            />
            {imported && id && !isImporting ? (
              <Button
                className={styles.importButton}
                label={intl.formatMessage(messages.openWalletLabel)}
                onClick={() => { onOpenWallet(id); }}
                skin={ButtonSkin}
              />
            ) : (
              <Button
                className={importButtonClasses}
                label={intl.formatMessage(messages.importLabel)}
                onClick={() => { onImportKeyFile(wallet); }}
                skin={ButtonSkin}
                disabled={password == null || isRestoreActive}
              />
            )}
          </div>
        );
      })
    );

    const submitButtonClasses = classnames([
      'primary',
      isMatchingPasswords ? styles.submitButtonSpinning : styles.submitButton,
    ]);

    return (
      <div className={styles.component}>

        <BorderedBox>

          <h2 className={styles.headline}>
            {intl.formatMessage(messages.headline)}
          </h2>

          <div className={styles.instructions}>
            <p>{intl.formatMessage(messages.instructions)}</p>
          </div>

          <div className={styles.fileUpload}>
            <FileUploadWidget
              {...keyFileField.bind()}
              selectedFile={keyFileField.value}
              showSelectedFilePath
              acceptedFileTypes=".key"
              onFileSelected={(file) => {
                // "set(value)" is an unbound method and thus must be explicitly called
                keyFileField.set(file);
                onSecretKeyFileSelect(file);
              }}
            />
          </div>

          {isExtractingWallets ? (
            <div className={styles.extractingWalletsWrapper}>
              <LoadingSpinner big />
              <p className={styles.extractingWalletsText}>
                {intl.formatMessage(messages.extractingWalletsNotification)}
              </p>
            </div>
          ) : null}

          {hasExtractedWallets && !wallets.length ? (
            <div className={styles.extractingWalletsWrapper}>
              <p className={styles.extractingWalletsText}>
                {intl.formatMessage(messages.noWalletsFoundNotification)}
              </p>
            </div>
          ) : null}

          {hasExtractedWallets && wallets.length ? (
            <div>
              <div className={styles.walletRows}>
                {generateWalletList()}
              </div>

              <div className={styles.maskPasswords}>
                <TinySwitch
                  label={intl.formatMessage(messages.maskPasswordsLabel)}
                  onChange={this.toggleMaskPasswords}
                  checked={maskPasswords}
                />
              </div>

              <TextArea
                className={styles.passwordsField}
                {...passwordsField.bind()}
                onClick={() => { this.unmaskPasswords(); }}
                onKeyDown={(e) => {
                  if (e.keyCode === 13) {
                    const numberOfLines = passwordsField.value.split('\n').length;
                    if (numberOfLines >= MAXIMUM_PASSWORD_COUNT) e.preventDefault();
                  }
                }}
                onPaste={() => {
                  setTimeout(() => {
                    const contentByLine = passwordsField.value.split('\n');
                    const truncatedContent = contentByLine.slice(0, MAXIMUM_PASSWORD_COUNT).join('\n');
                    passwordsField.set(truncatedContent);
                  });
                }}
                rows={5}
                skin={TextAreaSkin}
                readOnly={maskPasswords}
              />

              <Button
                className={submitButtonClasses}
                label={intl.formatMessage(messages.submitLabel)}
                onClick={submit}
                skin={ButtonSkin}
                disabled={isMatchingPasswords}
              />
            </div>
          ) : null}

        </BorderedBox>

      </div>
    );
  }

}
