// @flow
import { ipcRenderer } from 'electron';
import log from 'electron-log';
import moment from 'moment';

export const Logger = {

  debug: (data: string) => {
    log.debug(data);
  },

  info: (data: string) => {
    log.info(data);
  },

  error: (data: string) => {
    log.error(data);
    Logger.sendToRemote('error', data);
  },

  warn: (data: string) => {
    log.info(data);
  },

  sendToRemote: (type: string, data: string) => {
    // Log entry should be sent to the remote server in the format of:
    // [datestamp] [log-type]: log-data
    // e.g: [2017-08-22 11:25:20:0811] [debug] CardanoClientApi::getLocale called
    const logEntry = `${moment().format('YYYY-MM-DD HH:mm:ss:0SSS')} [${type}]: ${data}`;
    ipcRenderer.send('log-to-remote', logEntry);
  },

};

// ========== STRINGIFY =========

export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) => (
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2)
);

