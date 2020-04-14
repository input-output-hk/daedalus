// @flow
export type WalletExportTypeChoices = 'full' | 'readOnly' | 'paperWallet';

export type ExportedByronWallet = {
  encrypted_root_private_key: string,
  name: ?string,
  id: string,
  passphrase_hash: string,
};
