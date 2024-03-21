'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.decodeKeystore = void 0;
const cbor = __importStar(require('cbor'));
const blake2b = __importStar(require('blake2b'));
const crypto = __importStar(require('crypto'));
const decodeKeystore = async (bytes) => {
  return cbor.decodeAll(bytes).then((obj) => {
    /**
     * The original 'UserSecret' from cardano-sl looks like this:
     *
     * ```hs
     * data UserSecret = UserSecret
     *     { _usVss       :: Maybe VssKeyPair
     *     , _usPrimKey   :: Maybe SecretKey
     *     , _usKeys      :: [EncryptedSecretKey]
     *     , _usWalletSet :: Maybe WalletUserSecret
     *     , _usPath      :: FilePath
     *     , _usLock      :: Maybe FileLock
     *     }
     *
     * data WalletUserSecret = WalletUserSecret
     *     { _wusRootKey    :: EncryptedSecretKey
     *     , _wusWalletName :: Text
     *     , _wusAccounts   :: [(Word32, Text)]
     *     , _wusAddrs      :: [(Word32, Word32)]
     *     }
     * ```
     *
     * We are interested in:
     * - usKeys:
     *    which is where keys have been stored since ~2018
     *
     * - usWalletSet
     *    which seems to have been used in earlier version; at least the
     *    wallet from the time did allow to restore so-called 'wallets'
     *    from keys coming from that 'WalletUserSecret'
     */
    const usKeys = obj[0][2].map(toEncryptedSecretKey);
    const usWalletSet = obj[0][3].map((x) => toEncryptedSecretKey(x[0]));
    return usKeys.concat(usWalletSet);
  });
};
exports.decodeKeystore = decodeKeystore;
const toEncryptedSecretKey = ([encryptedPayload, passphraseHash]) => {
  const isEmptyPassphrase = $isEmptyPassphrase(passphraseHash);
  return {
    walletId: mkWalletId(encryptedPayload),
    encryptedPayload,
    passphraseHash,
    isEmptyPassphrase,
  };
};
const mkWalletId = (xprv) => {
  const xpub = xprv.slice(64);
  return blake2b(20).update(xpub).digest('hex');
};
const $isEmptyPassphrase = (pwd) => {
  const cborEmptyBytes = Buffer.from('40', 'hex');
  const [logN, r, p, salt, hashA] = pwd.toString('utf8').split('|');
  const opts = {
    N: 2 ** Number(logN),
    r: Number(r),
    p: Number(p),
  };
  // @ts-ignore
  const hashB = crypto
    .scryptSync(cborEmptyBytes, Buffer.from(salt, 'base64'), 32, opts)
    .toString('base64');
  return hashA === hashB;
};
//# sourceMappingURL=restoreKeystore.js.map
