'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.wrappedFind = exports.getProcess = exports.getProcessesByName = exports.getProcessName = exports.getProcessById = void 0;
const find_process_1 = __importDefault(require('find-process'));
const lodash_1 = require('lodash');
const path_1 = __importDefault(require('path'));
const logging_1 = require('./logging');
const getProcessById = async (processId) => {
  // finds running processes matching PID
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const matchingProcesses = await (0, exports.wrappedFind)('pid', processId);
  return matchingProcesses.length > 0 ? matchingProcesses[0] : Promise.reject();
};
exports.getProcessById = getProcessById;
const getProcessName = async (processId) =>
  (await (0, exports.getProcessById)(processId)).name;
exports.getProcessName = getProcessName;
const getProcessesByName = async (processName) => {
  // finds running processes matching name
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const matchingProcesses = await (0, exports.wrappedFind)('name', processName);
  return matchingProcesses;
};
exports.getProcessesByName = getProcessesByName;
const getProcess = async (processId, processName) => {
  try {
    // finds running processes matching PID
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
    const matchingProcesses = await (0, exports.wrappedFind)('pid', processId);
    // no processes exist with a matching PID
    if (!matchingProcesses.length) return null;
    // Return first matching process if names match
    const previousProcess = matchingProcesses[0];
    if ((0, lodash_1.isObject)(previousProcess)) {
      const binPathSegments = previousProcess.bin.split(path_1.default.sep);
      const exeName =
        binPathSegments.length > 0
          ? binPathSegments[binPathSegments.length - 1]
          : previousProcess.bin;
      if (exeName === processName || previousProcess.name === processName) {
        return previousProcess;
      }
    }
  } catch (error) {
    logging_1.logger.error('getProcess error', {
      error,
    });
    return null;
  }
};
exports.getProcess = getProcess;
/**
 * Like `find from 'find-process'`, but wraps the execution into setting locale to C (available anywhere).
 *
 * Inside Nix chroot for Linux installation, it is possible there will not be locale definitions for user’s LC_ALL,
 * which variable is inherited by the chroot from user’s regular system.
 *
 * Unfortunately, `/bin/sh` inside the Nix chroot outputs warnings to `stderr` about missing locales then.
 *
 * Then, `find-process` interprets non-empty stderr as some error, and throws.
 *
 * Another solution would be to patch `find-process` to construct the `ps` invocation itself, not using `/bin/sh`
 * for that (i.e. `exec('ps', 'aux')` instead of `exec('ps aux')` which is really `exec('/bin/sh', '-c', 'ps aux')`.
 */
const wrappedFind = async (type, value, strict) => {
  const previousLocale = process.env.LC_ALL;
  process.env.LC_ALL = 'C';
  const rv = await (0, find_process_1.default)(type, value, strict);
  if (typeof previousLocale !== 'undefined')
    process.env.LC_ALL = previousLocale;
  else delete process.env.LC_ALL;
  return rv;
};
exports.wrappedFind = wrappedFind;
//# sourceMappingURL=processes.js.map
