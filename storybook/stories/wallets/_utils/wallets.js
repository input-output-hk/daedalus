// @flow

// export type ExportedByronWallet = {
//   // Export tool wallet props
//   encrypted_root_private_key: string,
//   name: ?string,
//   id: string,
//   passphrase_hash: string,
//   is_passphrase_empty: boolean,

//   // Daedalus derived wallet props
//   hasName: boolean,
//   import: {
//     status: WalletImportStatus,
//     error: ?LocalizableError,
//   },
// };

// export type WalletImportStatus =
//   | 'unstarted'
//   | 'pending'
//   | 'running'
//   | 'completed'
//   | 'exists'
//   | 'errored';

export const exportedByronWallets = [
  {
    encrypted_root_private_key: '',
    name: 'Wallet 1',
    id: '1',
    passphrase_hash: '',
    is_passphrase_empty: false,
    hasName: true,
    import: {
      status: 'unstarted',
      error: null,
    },
  },
];
