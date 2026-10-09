import fs from 'fs-extra';
import path from 'path';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { EXPORT_WALLETS_CHANNEL } from '../../common/ipc/api';
import type {
  ExportWalletsRendererRequest,
  ExportWalletsMainResponse,
} from '../../common/ipc/api';
import { decodeKeystore } from '../utils/restoreKeystore';
import { logger } from '../utils/logging';

export const exportWalletsChannel: MainIpcChannel<
  ExportWalletsRendererRequest,
  ExportWalletsMainResponse
> = new MainIpcChannel(EXPORT_WALLETS_CHANNEL);

// Location of the key file inside a Daedalus state directory: Linux used
// "Secrets", macOS and Windows used "Secrets-1.0".
const STATE_DIR_SECRET_KEY_PATHS = [
  path.join('Secrets-1.0', 'secret.key'),
  path.join('Secrets', 'secret.key'),
];

const resolveSecretKeyPath = async (
  exportSourcePath: string
): Promise<string> => {
  const stats = await fs.stat(exportSourcePath);
  if (stats.isFile()) return exportSourcePath;
  for (const relativePath of STATE_DIR_SECRET_KEY_PATHS) {
    const candidate = path.join(exportSourcePath, relativePath);
    // eslint-disable-next-line no-await-in-loop
    if (await fs.pathExists(candidate)) return candidate;
  }
  throw new Error(`No secret.key found in ${exportSourcePath}`);
};

export const exportWallets = async ({
  exportSourcePath,
}: ExportWalletsRendererRequest): Promise<ExportWalletsMainResponse> => {
  logger.info('ipcMain: Starting wallets export...', { exportSourcePath });
  try {
    const secretKeyPath = await resolveSecretKeyPath(exportSourcePath);
    const rawWallets = await decodeKeystore(await fs.readFile(secretKeyPath));
    // The renderer derives hasName, import and index itself.
    const wallets = rawWallets.map((w) => ({
      name: null,
      id: w.walletId,
      isEmptyPassphrase: w.isEmptyPassphrase,
      passphrase_hash: w.passphraseHash.toString('hex'),
      encrypted_root_private_key: w.encryptedPayload.toString('hex'),
    })) as unknown as ExportWalletsMainResponse['wallets'];
    logger.info(`ipcMain: Exported ${wallets.length} wallets`, {
      walletsData: wallets.map((w) => ({
        id: w.id,
        hasPassword: !w.isEmptyPassphrase,
      })),
    });
    return { wallets, errors: '' };
  } catch (error) {
    logger.error('ipcMain: Exporting wallets failed', { error });
    return { wallets: [], errors: String(error) };
  }
};

export const handleExportWalletsRequests = () => {
  exportWalletsChannel.onRequest(exportWallets);
};
