import * as util from 'util';
import * as fs from 'fs';

import chalk from 'chalk';

import { Logger } from './logging';
export { Logger, LogFunc } from './logging';

export class StdioLogger implements Logger {
  constructor(
    readonly options: { fd: number; prefix?: string; timestamps?: boolean } = {
      fd: process.stdout.fd,
    }
  ) {}

  protected write(level: string, message: string, param?: unknown) {
    const suffix = param ? util.format(' %o', param) : '';
    const ts = this.options.timestamps
      ? chalk.dim('[' + new Date().toISOString() + ']') + ' '
      : '';
    const prefix = this.options.prefix ? this.options.prefix + ' ' : '';
    const line = `${level} ${prefix}${ts}${message}${suffix}\n`;
    fs.writeSync(this.options.fd, line);
  }
  debug(msg: string, param?: unknown) {
    this.write(chalk.dim('DEBUG'), msg, param);
  }
  info(msg: string, param?: unknown) {
    this.write(chalk.whiteBright('INFO '), msg, param);
  }
  error(msg: string, param?: unknown) {
    this.write(chalk.red('ERROR'), msg, param);
  }
  log(msg: string, param?: unknown) {
    this.write('     ', msg, param);
  }
}
