/**
 * `node:sqlite` ships with the Node that Electron 41.3.0 embeds, but the
 * `@types/node` in this tree predates the module, so nothing declares it.
 *
 * This covers the surface `source/main/assets` actually uses and no more. A
 * declaration wider than its use is a claim nothing checks, and this file is
 * the only thing standing between a typo in a column binding and a runtime
 * failure three modules away.
 */
declare module 'node:sqlite' {
  export type SQLInputValue = null | number | bigint | string | Uint8Array;
  export type SQLOutputValue = null | number | bigint | string | Uint8Array;

  export class StatementSync {
    all(...params: Array<SQLInputValue>): Array<Record<string, SQLOutputValue>>;
    get(
      ...params: Array<SQLInputValue>
    ): Record<string, SQLOutputValue> | undefined;
    run(...params: Array<SQLInputValue>): {
      changes: number;
      lastInsertRowid: number | bigint;
    };
  }

  export class DatabaseSync {
    constructor(location: string, options?: { readOnly?: boolean });
    close(): void;
    exec(sql: string): void;
    prepare(sql: string): StatementSync;
  }
}
