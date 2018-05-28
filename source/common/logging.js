// @flow
import log from 'electron-log';

export const Logger = {

  debug: (data: string) => {
    log.debug(data);
  },

  info: (data: string) => {
    log.info(data);
  },

  error: (data: string) => {
    log.error(data);
  },

  warn: (data: string) => {
    log.info(data);
  },

};

// ========== STRINGIFY =========

export const stringifyData = (data: any) => JSON.stringify(data, null, 2);

export const stringifyError = (error: any) => (
  JSON.stringify(error, Object.getOwnPropertyNames(error), 2)
);

