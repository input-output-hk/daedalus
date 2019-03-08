// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { TextArea } from 'react-polymorph/lib/components/TextArea';
import { TextAreaSkin } from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import cbor from 'cbor';
import scrypt from 'scryptsy';
import BorderedBox from '../widgets/BorderedBox';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import FileUploadWidget from '../widgets/forms/FileUploadWidget';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './WalletImporter.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { encryptPassphrase } from '../../api/utils';
import { DOWNLOAD_KEY_FILE } from '../../../../common/ipc-api';
// import { scryptSync } from 'crypto';

const { ipcRenderer } = global;

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
  passwordsListHint: {
    id: 'wallet.importer.passwordsListHint',
    defaultMessage: '!!!Enter your potential wallet passwords line by line',
    description: 'Hint for the passwords list field on the wallet importer page.'
  },
  submitLabel: {
    id: 'wallet.importer.submitLabel',
    defaultMessage: '!!!Analyse passwords',
    description: 'Label for the "Analyse passwords" submit button on the wallet importer page.'
  },
  extractingWalletsNotification: {
    id: 'wallet.importer.extractingWalletsNotification',
    defaultMessage: '!!!Extracting wallets',
    description: 'Notification shown during wallet extraction on the wallet importer page.'
  },
  noWalletsFoundNotification: {
    id: 'wallet.importer.noWalletsFoundNotification',
    defaultMessage: '!!!No wallets found in the selected secrets key file.',
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
    defaultMessage: '!!!no password',
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
});

type Props = {};

type State = {
  isExtractingWallets: boolean,
  isSubmitting: boolean,
  wallets: Array<{
    raw: Array<Buffer>,
    passwordHash: string,
    password: ?string,
    balance: ?number,
  }>,
};

@observer
export default class WalletImporter extends Component<Props, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isExtractingWallets: false,
    isSubmitting: false,
    wallets: [],
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

  submit = () => {
    const { form } = this;
    const passwordsField = form.$('passwords');
    this.setState({ isSubmitting: true });
    setTimeout(() => {
      const passwords = passwordsField.value.split('\n');
      passwords.forEach((password) => { this.testPassword(password); });
      this.setState({ isSubmitting: false });
    }, 100);
  };

  checkPassword(passphraseHash: string, passhash: Buffer) {
    const bits = passphraseHash.split('|');
    const logN = parseInt(bits[0], 10);
    const r = parseInt(bits[1], 10);
    const p = parseInt(bits[2], 10);
    const salt = Buffer.from(bits[3], 'base64');
    const realhash = Buffer.from(bits[4], 'base64');

    // from nodejs native crypto (which is missing in renderer proc)
    // var hash2 = scryptSync(cbor.encode(passhash), salt, 32, { N: 2 ** logN, r: r, p: p });
    // try the scryptsy from npm (works, but worse performance)
    const hash2 = scrypt(cbor.encode(passhash), salt, 2 ** logN, r, p, 32);
    return realhash.equals(hash2);
  }

  testPassword(password: string) {
    const { wallets } = this.state;
    const testPasswordHash = password === '' ?
      Buffer.from([]) : Buffer.from(encryptPassphrase(password), 'hex');
    wallets.forEach((wallet) => {
      if (this.checkPassword(wallet.passwordHash, testPasswordHash)) {
        wallet.password = password;
      }
    });
    this.setState({ wallets });
  }

  extractKey(wallet: Object) {
    // the WalletUserSecret
    const wus = [];
    wus[0] = wallet.raw;
    wus[1] = 'Imported Wallet';
    wus[2] = []; // accounts
    wus[3] = []; // addresses
    // the UserSecret
    const newSecrets = cbor.encode([[], [], [], [wus]]);
    return newSecrets;
  }

  downloadKeyFile = (fileName: string, wallet: Object) => {
    const filePath = global.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (filePath) {
      const { extractKey } = this;
      const fileContent = extractKey(wallet);
      ipcRenderer.send(DOWNLOAD_KEY_FILE.REQUEST, fileName, fileContent, filePath);
    }
  };

  render() {
    const { intl } = this.context;
    const { isSubmitting, isExtractingWallets, wallets } = this.state;
    const { form, submit, downloadKeyFile } = this;
    const keyFileField = form.$('keyFile');
    const passwordsField = form.$('passwords');

    const hasExtractedWallets = keyFileField.value && !isExtractingWallets;

    const generateWalletList = () => {
      const walletList = [];
      wallets.forEach((wallet, index) => {
        const { password, balance } = wallet;
        const fileName = `wallet-${index + 1}.key${password !== '' ? '.locked' : ''}`;
        walletList.push(
          <div className={styles.walletRow}>
            <Input
              label={!index ? intl.formatMessage(messages.walletFileLabel) : null}
              value={fileName}
              onClick={() => { downloadKeyFile(fileName, wallet); }}
              skin={InputSkin}
              readOnly
            />
            <Input
              label={!index ? intl.formatMessage(messages.walletPasswordLabel) : null}
              placeholder={intl.formatMessage(messages.unknownPasswordHint)}
              value={password === '' ? intl.formatMessage(messages.noPasswordHint) : password}
              skin={InputSkin}
              readOnly
            />
            <Input
              label={!index ? intl.formatMessage(messages.walletBalanceLabel) : null}
              placeholder={intl.formatMessage(messages.unknownBalanceHint)}
              value={balance}
              skin={InputSkin}
              readOnly
            />
            <Button
              className={styles.importButton}
              label={intl.formatMessage(messages.importLabel)}
              skin={ButtonSkin}
              disabled
            />
          </div>
        );
      });
      return walletList;
    };

    const submitButtonClasses = classnames([
      'primary',
      isSubmitting ? styles.submitButtonSpinning : styles.submitButton,
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

                this.setState({ isExtractingWallets: true });

                // from https://react-dropzone.netlify.com/
                const reader = new FileReader();
                reader.onabort = () => console.log('file reading was aborted');
                reader.onerror = () => console.log('file reading has failed');
                reader.onload = () => {
                  const binaryStr = Buffer.from(reader.result);
                  const decodedSecrets = cbor.decode(binaryStr);
                  const keys = decodedSecrets[2];
                  const walletHashes = [];
                  keys.forEach((key) => {
                    walletHashes.push({
                      raw: key,
                      passwordHash: key[1].toString('ascii'),
                      password: null,
                      balance: null,
                    });
                  });
                  this.setState({
                    isExtractingWallets: false,
                    wallets: walletHashes,
                  });
                  this.testPassword('');
                };

                setTimeout(() => { reader.readAsArrayBuffer(file); }, 100);
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

              <TextArea
                {...passwordsField.bind()}
                rows={5}
                skin={TextAreaSkin}
              />

              <Button
                className={submitButtonClasses}
                label={intl.formatMessage(messages.submitLabel)}
                onClick={submit}
                skin={ButtonSkin}
              />
            </div>
          ) : null}

        </BorderedBox>

      </div>
    );
  }

}
