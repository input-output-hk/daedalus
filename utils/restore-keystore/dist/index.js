"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.prettyKeystore = exports.decodeKeystore = void 0;
const cbor = require("cbor");
const blake2b = require("blake2b");
const crypto = require("crypto");
function decodeKeystore(bytes) {
    return cbor
        .decodeAll(bytes)
        .then(obj => obj[0][2].map(toEncryptedSecretKey));
}
exports.decodeKeystore = decodeKeystore;
function prettyKeystore(keystore) {
    const prettyESK = ({ encryptedPayload, passphraseHash, walletId, isEmptyPassphrase }) => {
        return {
            name: walletId,
            encrypted_root_private_key: encryptedPayload.toString('hex'),
            passphrase_hash: passphraseHash.toString('hex'),
            is_empty_passphrase: isEmptyPassphrase
        };
    };
    return JSON.stringify(keystore.map(prettyESK), null, 4);
}
exports.prettyKeystore = prettyKeystore;
function toEncryptedSecretKey([encryptedPayload, passphraseHash]) {
    const isEmptyPassphrase = $isEmptyPassphrase(passphraseHash);
    return {
        walletId: mkWalletId(encryptedPayload),
        encryptedPayload,
        passphraseHash,
        isEmptyPassphrase,
    };
}
function mkWalletId(xprv) {
    const xpub = xprv.slice(64);
    return blake2b(20).update(xpub).digest('hex');
}
function $isEmptyPassphrase(pwd) {
    const cborEmptyBytes = Buffer.from('40', 'hex');
    const [logN, r, p, salt, hashA] = pwd.toString('utf8').split('|');
    const opts = { N: 2 ** Number(logN), r: Number(r), p: Number(p) };
    const hashB = crypto.scryptSync(cborEmptyBytes, Buffer.from(salt, 'base64'), 32, opts).toString('base64');
    return hashA === hashB;
}
//# sourceMappingURL=index.js.map