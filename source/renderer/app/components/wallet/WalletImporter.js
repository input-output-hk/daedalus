// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import cbor from 'cbor';
import scrypt from 'scryptsy';
import BorderedBox from '../widgets/BorderedBox';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import FileUploadWidget from '../widgets/forms/FileUploadWidget';
import styles from './WalletImporter.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { submitOnEnter } from '../../utils/form';
import { encryptPassphrase } from '../../api/utils';
// import { scryptSync } from 'crypto';


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
  spendingPasswordLabel: {
    id: 'wallet.restore.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Enter password',
    description: 'Label for the "Wallet password" input in the wallet restore dialog.',
  },
  passwordFieldPlaceholder: {
    id: 'wallet.restore.dialog.passwordFieldPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for the "Password" inputs in the wallet restore dialog.',
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
      walletHashes: {
        value: [],
      },
      confirmedPasswordHashes: {
        value: [],
      },
      spendingPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.spendingPasswordLabel),
        placeholder: this.context.intl.formatMessage(messages.passwordFieldPlaceholder),
        value: '',
      },
    },
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
    },
  });

  submit() {
    const { form } = this;
    const spendingPasswordField = form.$('spendingPassword');
    console.log(spendingPasswordField.value);
    this.testPassword(spendingPasswordField.value);
  }

  checkPassword(passphraseHash, passhash) {
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

  testPassword(password) {
    const { form } = this;
    const walletHashesField = form.$('walletHashes');
    const confirmedPasswordHashesField = form.$('confirmedPasswordHashes');

    let testpasshash = null;
    if (password === '') testpasshash = Buffer.from([]);
    else testpasshash = Buffer.from(encryptPassphrase(password), 'hex');

    for (let idx = 0; idx < walletHashesField.value.length; idx++) {
      if (this.checkPassword(walletHashesField.value[idx].pwhash, testpasshash)) {
        console.log('wallet idx', idx, 'has password', password);
        const oldhashes = confirmedPasswordHashesField.value;
        oldhashes[idx] = testpasshash.toString('hex');
        confirmedPasswordHashesField.set(oldhashes);
      }
    }
  }
  extractKey(idx) {
    const { form } = this;
    const walletHashesField = form.$('walletHashes');
    const input = walletHashesField.value[idx];
    // the WalletUserSecret
    const wus = [];
    wus[0] = input.raw;
    wus[1] = 'extracted key#' + idx;
    wus[2] = []; // accounts
    wus[3] = []; // addresses
    // the UserSecret
    const newSecrets = cbor.encode([[], [], [], [wus]]);
    return newSecrets;
  }

  render() {
    const { intl } = this.context;
    const { form } = this;
    const keyFileField = form.$('keyFile');
    const walletHashesField = form.$('walletHashes');
    const spendingPasswordField = form.$('spendingPassword');
    const confirmedPasswordHashesField = form.$('confirmedPasswordHashes');

    function generateWalletList() {
      const knownHashes = confirmedPasswordHashesField.value;
      let x = <div>null dom element</div>;
      for (let idx = 0; idx < knownHashes.length; idx++) {
        x += <div>wallet#{idx}</div>;
        if (knownHashes[idx] == null) {
          console.log('wallet#' + idx + ' has unknown pw');
        } else {
          console.log('wallet#' + idx + ' has pw hash ' + knownHashes[idx]);
          console.log(this.extractKey(idx));
        }
      }
      return x;
    }

    return (
      <div className={styles.component}>

        <BorderedBox>

          <h1 className={styles.headline}>{intl.formatMessage(messages.headline)}</h1>

          <div className={styles.instructions}>
            <p>{intl.formatMessage(messages.instructions)}</p>
            <b>{keyFileField.value.path}</b>
          </div>

          <div className={styles.fileUpload}>
            <FileUploadWidget
              {...keyFileField.bind()}
              selectedFile={keyFileField.value}
              acceptedFileTypes=".key"
              onFileSelected={(file) => {
                // "set(value)" is an unbound method and thus must be explicitly called
                keyFileField.set(file);
                // from https://react-dropzone.netlify.com/
                const reader = new FileReader();
                reader.onabort = () => console.log('file reading was aborted');
                reader.onerror = () => console.log('file reading has failed');
                reader.onload = () => {
                  const binaryStr = Buffer.from(reader.result);
                  const decodedSecrets = cbor.decode(binaryStr);
                  const keys = decodedSecrets[2];
                  const walletHashes = [];
                  const pwhashes = [];
                  for (let x = 0; x < keys.length; x++) {
                    walletHashes[x] = { raw: keys[x], pwhash: keys[x][1].toString('ascii') };
                    pwhashes[x] = null;
                  }
                  walletHashesField.set(walletHashes);
                  confirmedPasswordHashesField.set(pwhashes);
                  this.testPassword('');
                };
                reader.readAsArrayBuffer(file);
              }}
            />
          </div>

          {walletHashesField.value !== [] ? (
            generateWalletList.apply(this)
          ) : (
            <div />
          )}

          {keyFileField.value ? (
            <div>
              <Input
                className="spendingPassword"
                onKeyPress={submitOnEnter.bind(this, this.submit.bind(this))}
                {...spendingPasswordField.bind()}
                error={spendingPasswordField.error}
                skin={InputSkin}
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
