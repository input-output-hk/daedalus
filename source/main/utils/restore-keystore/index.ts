import * as cbor from 'cbor';
import * as blake2b from 'blake2b';
import * as crypto from 'crypto';

export type EncryptedSecretKey = {
  encryptedPayload,
  passphraseHash: Buffer,
  isEmptyPassphrase: Boolean,
  walletId: WalletId
};

export type WalletId = String;

export function decodeKeystore (bytes : Buffer) : Promise<[EncryptedSecretKey]> {
  return cbor
    .decodeAll(bytes)
    .then(obj => obj[0][2].map(toEncryptedSecretKey));
}

export function prettyKeystore(keystore : [EncryptedSecretKey]) : String {
  const prettyESK = ({ encryptedPayload, passphraseHash, walletId, isEmptyPassphrase }) => {
    return {
      name: walletId,
      encrypted_root_private_key: encryptedPayload.toString('hex'),
      passphrase_hash: passphraseHash.toString('hex'),
      is_empty_passphrase: isEmptyPassphrase
    }
  };
  return JSON.stringify(keystore.map(prettyESK), null, 4);
}

//
// Internals
//

function toEncryptedSecretKey ([encryptedPayload,passphraseHash] : [Buffer,Buffer]) : EncryptedSecretKey {
  const isEmptyPassphrase = $isEmptyPassphrase(passphraseHash);
  return {
    walletId: mkWalletId(encryptedPayload),
    encryptedPayload,
    passphraseHash,
    isEmptyPassphrase,
  };
}

function mkWalletId (xprv : Buffer) : WalletId {
  const xpub = xprv.slice(64);
  return blake2b(20).update(xpub).digest('hex');
}

function $isEmptyPassphrase (pwd : Buffer) : Boolean {
  const cborEmptyBytes = Buffer.from('40', 'hex');
  const [ logN, r, p, salt, hashA ] = pwd.toString('utf8').split('|');
  const opts = { N: 2 ** Number(logN), r: Number(r), p: Number(p) };
  const hashB = crypto.scryptSync(cborEmptyBytes, Buffer.from(salt, 'base64'), 32, opts).toString('base64');
  return hashA === hashB;
}
