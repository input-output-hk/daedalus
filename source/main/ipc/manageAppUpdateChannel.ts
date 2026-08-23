import { app, shell } from 'electron';
import fs from 'fs';
import shasum from 'shasum';
import type { BrowserWindow } from 'electron';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { MANAGE_APP_UPDATE } from '../../common/ipc/api';
import type {
  ManageAppUpdateRendererRequest as Request,
  ManageAppUpdateMainResponse as Response,
} from '../../common/ipc/api';
import { UPDATE_INSTALLATION_STATUSES as statuses } from '../../common/config/appUpdateConfig';
import { environment } from '../environment';
import { logger } from '../utils/logging';
import { launcherConfig } from '../config';
import {
  consumeIpcResponse,
  currentWindowSender,
} from './lib/currentWindowSender';
// IpcChannel<Incoming, Outgoing>
const manageAppUpdateChannel: MainIpcChannel<
  Request,
  Response
> = new MainIpcChannel(MANAGE_APP_UPDATE);
const logPrefix = 'appUpdateInstall';

const getMessage = (functionPrefix: string, message?: string): string => {
  let formattedMessage = `${logPrefix}:${functionPrefix}`;
  if (message) formattedMessage += `: ${message}`;
  return formattedMessage;
};

export const handleManageAppUpdateRequests = (
  _window: Pick<BrowserWindow, 'close'>
) => {
  const response = (
    success: boolean | null | undefined,
    functionPrefix: string,
    messageText = '',
    _data: Record<string, unknown> = {}
  ): Response => {
    let status = statuses.PROGRESS;
    if (success === true) status = statuses.SUCCESS;
    else if (success === false) status = statuses.ERROR;
    const log = success === false ? logger.error : logger.info;
    const message = getMessage(functionPrefix, messageText);
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    log(getMessage(functionPrefix, message));
    const data = { ..._data, message };
    consumeIpcResponse(
      manageAppUpdateChannel.send(
        {
          status,
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ status: UpdateInstallationStat... Remove this comment to see the full error message
          message,
          data,
        },
        currentWindowSender.sender
      ),
      MANAGE_APP_UPDATE
    );
    return {
      status,
      // @ts-ignore ts-migrate(2322) FIXME: Type '{ status: UpdateInstallationStatus; message:... Remove this comment to see the full error message
      message,
      data,
    };
  };

  const checkInstallerHash = (filePath, expectedHash): boolean => {
    const { name: functionPrefix } = checkInstallerHash;
    const fileBuffer = fs.readFileSync(filePath);

    if (!fileBuffer) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.error(getMessage(functionPrefix, 'Unable to read the installer:'));
      return false;
    }

    const fileHash = shasum(fileBuffer, 'sha256');

    if (fileHash !== expectedHash) {
      logger.error(getMessage(functionPrefix, 'Hash does not match'), {
        filePath,
      });
      return false;
    }

    return true;
  };

  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ filePath, hash: expectedHash ... Remove this comment to see the full error message
  manageAppUpdateChannel.onRequest(async ({ filePath, hash: expectedHash }) => {
    const functionPrefix = 'onRequest';
    if (
      environment.isLinux ||
      launcherConfig.applicationUpdateMode === 'system-package-disabled'
    ) {
      return response(
        false,
        functionPrefix,
        'Application updates must be installed manually with the system package manager.',
        { info: { reason: 'system-package-update-disabled' } }
      );
    }
    const fileExists = fs.existsSync(filePath);
    if (!fileExists)
      return response(false, functionPrefix, 'Installer not found:', {
        info: {
          filePath,
        },
      });
    const installerHash = checkInstallerHash(filePath, expectedHash);
    if (!installerHash) return response(false, functionPrefix);
    // macOS and Windows open the verified installer after the app has closed.
    app.on('quit', () => {
      shell.openPath(filePath);
    });
    app.quit();
    return response(true, functionPrefix);
  });
};
