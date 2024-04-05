// Copyright © 2020 IOHK
// License: Apache-2.0

/**
 * Cheap and cheerful logging functions.
 * Same as what is already in Daedalus.
 * Plug in your own logging by implementing [[Logger]].
 *
 * @packageDocumentation
 /*

/**
 * Logging adapter.
 */
export interface Logger {
  debug: LogFunc;
  info: LogFunc;
  error: LogFunc;
  log: LogFunc;
}

export type Severity = 'debug' | 'log' | 'info' | 'error';

/**
 * Function which logs a message and optional object.
 */
export interface LogFunc {
  (msg: string, param?: unknown): void;
}

/**
 * Create a new logger with a context name added.
 *
 * @param logger - existing logger.
 * @param name - context to prepend.
 * @return - a new logger.
 */
export function prependName(logger: Logger, name: string): Logger {
  const prefix = (severity: Severity, msg: string, param?: unknown): void => {
    const prefixed = `${name}: ${msg}`;
    if (param) {
      logger[severity](prefixed, param);
    } else {
      logger[severity](prefixed);
    }
  };
  return {
    debug: (msg: string, param?: unknown): void => prefix('debug', msg, param),
    log: (msg: string, param?: unknown): void => prefix('log', msg, param),
    info: (msg: string, param?: unknown): void => prefix('info', msg, param),
    error: (msg: string, param?: unknown): void => prefix('error', msg, param),
  };
}
