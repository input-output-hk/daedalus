import events from 'events';
import fs from 'fs';
import path from 'path';
import { logger as defaultLogger } from './logging';
import type { Logger } from '../../common/types/logging.types';

const ENCODING = 'utf-8';

export class Tail extends events.EventEmitter {
  filename: string;
  buffer: string;
  internalDispatcher: events.EventEmitter;
  queue: { start: number; end: number }[];
  isWatching: boolean;
  currentCursorPos: number;
  watcher: fs.FSWatcher;
  rewatchId: NodeJS.Timeout;
  logger: Logger;

  constructor(filename: string, logger: Logger = defaultLogger) {
    super();
    this.filename = filename;
    this.logger = logger;

    this.logger.info(`Tail starting...`);
    this.logger.info(`filename: ${this.filename}`);

    try {
      fs.accessSync(this.filename, fs.constants.F_OK);
    } catch (err) {
      if (err.code === 'ENOENT') {
        throw err;
      }
    }

    this.buffer = '';
    this.internalDispatcher = new events.EventEmitter();
    this.queue = [];
    this.isWatching = false;

    this.internalDispatcher.on('next', () => {
      this.readBlock();
    });

    try {
      this.watch(this.latestPosition());
    } catch (err) {
      this.logger.error(`watch for ${this.filename} failed: ${err}`);
      this.emit('error', `watch for ${this.filename} failed: ${err}`);
    }
  }

  latestPosition() {
    try {
      return fs.statSync(this.filename).size;
    } catch (err) {
      this.logger.error(`size check for ${this.filename} failed: ${err}`);
      this.emit('error', `size check for ${this.filename} failed: ${err}`);
      throw err;
    }
  }

  readBlock() {
    if (this.queue.length >= 1) {
      const block = this.queue[0];
      if (block.end > block.start) {
        const stream = fs.createReadStream(this.filename, {
          start: block.start,
          end: block.end - 1,
          encoding: ENCODING,
        });
        stream.on('error', (error) => {
          this.logger.error(`Tail error: ${error}`);
          this.emit('error', error);
        });
        stream.on('end', () => {
          this.queue.shift();
          if (this.queue.length > 0) {
            this.internalDispatcher.emit('next');
          }
        });
        stream.on('data', (d) => {
          this.buffer += d;
          const parts = this.buffer.split(/[\r]{0,1}\n/);
          this.buffer = parts.pop();
          for (const chunk of parts) {
            this.emit('line', chunk);
          }
        });
      }
    }
  }

  change() {
    const p = this.latestPosition();
    if (p < this.currentCursorPos) {
      this.currentCursorPos = p;
    } else if (p > this.currentCursorPos) {
      this.queue.push({ start: this.currentCursorPos, end: p });
      this.currentCursorPos = p;
      if (this.queue.length === 1) {
        this.internalDispatcher.emit('next');
      }
    }
  }

  watch(startingCursor) {
    if (this.isWatching) return;
    this.logger.info(`filesystem.watch present? ${fs.watch !== undefined}`);

    this.isWatching = true;
    this.currentCursorPos = startingCursor;

    if (fs.watch) {
      this.logger.info(`watch strategy: watch`);
      this.watcher = fs.watch(this.filename, {}, (e, filename) => {
        this.watchEvent(e, filename);
      });
    } else {
      this.logger.info(`watch strategy: watchFile`);
      fs.watchFile(this.filename, {}, (curr, prev) => {
        this.watchFileEvent(curr, prev);
      });
    }
  }

  rename(filename) {
    if (filename === undefined || filename !== this.filename) {
      this.unwatch();
      this.filename = path.join(path.dirname(this.filename), filename);
      this.rewatchId = setTimeout(() => {
        try {
          this.watch(this.currentCursorPos);
        } catch (ex) {
          this.logger.error(
            `'rename' event for ${this.filename}. File not available anymore.`
          );
          this.emit('error', ex);
        }
      }, 1000);
    } else {
      this.logger.info('rename event but same filename');
    }
  }

  watchEvent(e, evtFilename) {
    try {
      if (e === 'change') {
        this.change();
      } else if (e === 'rename') {
        this.rename(evtFilename);
      }
    } catch (err) {
      this.logger.error(`watchEvent for ${this.filename} failed: ${err}`);
      this.emit('error', `watchEvent for ${this.filename} failed: ${err}`);
    }
  }

  watchFileEvent(curr, prev) {
    if (curr.size > prev.size) {
      this.currentCursorPos = curr.size;
      this.queue.push({ start: prev.size, end: curr.size });
      if (this.queue.length === 1) {
        this.internalDispatcher.emit('next');
      }
    }
  }

  unwatch() {
    if (this.watcher) {
      this.watcher.close();
    } else {
      fs.unwatchFile(this.filename);
    }
    if (this.rewatchId) {
      clearTimeout(this.rewatchId);
      this.rewatchId = undefined;
    }
    this.isWatching = false;
    this.queue = [];
    this.logger.info(`Unwatch ${this.filename}`);
  }
}
