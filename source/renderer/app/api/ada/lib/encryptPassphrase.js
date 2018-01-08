// @flow
import blakejs from 'blakejs';

const bytesToB16 = (bytes) => Buffer.from(bytes).toString('hex');
const blake2b = (data) => blakejs.blake2b(data, null, 32);

export const encryptPassphrase = (passphrase: string) => (
  bytesToB16(blake2b(passphrase))
);
