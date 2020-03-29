// @flow
export type WalletExportTypeChoices = 'full' | 'readOnly' | 'paperWallet';

export type ExportedByronWallet = {
  encrypted_root_private_key: string,
  name: string,
  passphrase_hash: string,
};
