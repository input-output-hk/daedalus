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
export declare function prependName(logger: Logger, name: string): Logger;
