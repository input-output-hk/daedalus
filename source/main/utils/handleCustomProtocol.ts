// @flow
import { BrowserWindow } from 'electron';
import { getCustomProtocolChannel } from '../ipc/getCustomProtocolChannel';
import { logger } from './logging';

export const handleCustomProtocol = (
  url: string,
  mainWindow: BrowserWindow
) => {
  try {
    logger.info(`[Custom-Protocol] Send URL query params ur=${url}`);
    getCustomProtocolChannel.send(url, mainWindow.webContents);
  } catch (error) {
    logger.info('[Custom-Protocol] Send URL query params ERROR', { error });
    throw error;
  }
};
