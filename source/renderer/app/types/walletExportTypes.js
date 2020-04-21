// @flow
import LocalizableError from '../i18n/LocalizableError';

export type WalletExportTypeChoices = 'full' | 'readOnly' | 'paperWallet';

export type WalletImportStatus =
  | 'unstarted'
  | 'pending'
  | 'running'
  | 'completed'
  | 'exists'
  | 'errored';

export const WalletImportStatuses: {
  UNSTARTED: WalletImportStatus,
  PENDING: WalletImportStatus,
  RUNNING: WalletImportStatus,
  COMPLETED: WalletImportStatus,
  EXISTS: WalletImportStatus,
  ERRORED: WalletImportStatus,
} = {
  UNSTARTED: 'unstarted',
  PENDING: 'pending',
  RUNNING: 'running',
  COMPLETED: 'completed',
  EXISTS: 'exists',
  ERRORED: 'errored',
};

export type ExportedByronWallet = {
  // Export tool wallet props
  encrypted_root_private_key: string,
  name: ?string,
  id: string,
  passphrase_hash: string,
  is_passphrase_empty: boolean,

  // Daedalus derived wallet props
  hasName: boolean,
  import: {
    status: WalletImportStatus,
    error: ?LocalizableError,
  },
  index: number,
};
