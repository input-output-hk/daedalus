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
}

/**
 * Function which logs a message and optional object.
 */
export interface LogFunc {
  (msg: string, param?: any): void;
}

/**
 * Create a new logger with a context name added.
 *
 * @param logger - existing logger.
 * @param name - context to prepend.
 * @return - a new logger.
 */
export function prependName(logger: Logger, name: string): Logger {
  const prefix = (
    severity: 'debug' | 'info' | 'error',
    msg: string,
    param?: any
  ) => {
    const prefixed = `${name}: ${msg}`;
    if (param) {
      logger[severity](prefixed, param);
    } else {
      logger[severity](prefixed);
    }
  };
  return {
    debug: (msg: string, param?: any) => prefix('debug', msg, param),
    info: (msg: string, param?: any) => prefix('info', msg, param),
    error: (msg: string, param?: any) => prefix('error', msg, param),
  };
}
