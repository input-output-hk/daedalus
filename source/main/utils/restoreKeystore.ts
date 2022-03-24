import * as cbor from 'cbor';
import * as blake2b from 'blake2b';
import * as crypto from 'crypto';

export type EncryptedSecretKeys = Array<EncryptedSecretKey>;
export type EncryptedSecretKey = {
  encryptedPayload: Buffer;
  passphraseHash: Buffer;
  isEmptyPassphrase: boolean;
  walletId: WalletId;
};
export type WalletId = string;
export const decodeKeystore = async (
  bytes: Buffer
): Promise<EncryptedSecretKeys> => {
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

const toEncryptedSecretKey = ([encryptedPayload, passphraseHash]: [
  Buffer,
  Buffer
]): EncryptedSecretKey => {
  const isEmptyPassphrase = $isEmptyPassphrase(passphraseHash);
  return {
    walletId: mkWalletId(encryptedPayload),
    encryptedPayload,
    passphraseHash,
    isEmptyPassphrase,
  };
};

const mkWalletId = (xprv: Buffer): WalletId => {
  const xpub = xprv.slice(64);
  return blake2b(20).update(xpub).digest('hex');
};

const $isEmptyPassphrase = (pwd: Buffer): boolean => {
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
