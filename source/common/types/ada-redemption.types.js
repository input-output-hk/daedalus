// @flow

type Passphrase = string;
type DecryptionKey = string;
type ForceVendingDecryptionKey = [?string, ?string, ?string];

export type AdaRedemptionDecryptionKey =
  | Passphrase
  | DecryptionKey
  | ForceVendingDecryptionKey;

export type AdaRedemptionCode = string;
