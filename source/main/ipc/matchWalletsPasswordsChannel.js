// @flow
import cbor from 'cbor';
import scrypt from 'scrypt-async';
import { encryptPassphrase } from '../../common/crypto/encrypt';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { MatchWalletsPasswordsChannelName } from '../../common/ipc/api';
import type {
  MatchWalletsPasswordsRendererRequest,
  MatchWalletsPasswordsMainResponse
} from '../../common/ipc/api';

export const matchWalletsPasswordsChannel: (
  // IpcChannel<Incoming, Outgoing>
  MainIpcChannel<MatchWalletsPasswordsRendererRequest, MatchWalletsPasswordsMainResponse>
) = (
  new MainIpcChannel(MatchWalletsPasswordsChannelName)
);

export const handleMatchWalletsPasswordsRequests = () => {
  const checkPassword = (passphraseHash: string, passhash: Buffer) => {
    const bits = passphraseHash.split('|');
    const logN = parseInt(bits[0], 10);
    const r = parseInt(bits[1], 10);
    const p = parseInt(bits[2], 10);
    const salt = Buffer.from(bits[3], 'base64');
    const realhash = Buffer.from(bits[4], 'base64');

    let hash2;
    scrypt(cbor.encode(passhash), salt, {
      N: 2 ** logN, r, p, dkLen: 32, encoding: 'binary',
    }, (key) => { hash2 = key; });

    return realhash.equals(hash2);
  };

  matchWalletsPasswordsChannel.onReceive((request: MatchWalletsPasswordsRendererRequest) => (
    new Promise((resolve) => {
      const { wallets, passwords } = request;

      wallets.forEach((wallet) => {
        if (wallet.password == null) {
          passwords.forEach((testPassword) => {
            if (wallet.password == null) {
              const testPasswordHash = testPassword === '' ?
                Buffer.from([]) : Buffer.from(encryptPassphrase(testPassword), 'hex');
              if (checkPassword(wallet.passwordHash, testPasswordHash)) {
                wallet.password = testPassword;
              }
            }
          });
        }
      });

      resolve(wallets);
    })
  ));
};
