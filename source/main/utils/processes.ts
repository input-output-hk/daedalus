import find from 'find-process';
import { isObject } from 'lodash';
import path from 'path';
import { logger } from './logging';

/* eslint-disable consistent-return */
export type Process = {
  pid: number;
  ppid: number;
  name: string;
  cmd: string;
  bin: string;
};
export const getProcessById = async (processId: number): Promise<Process> => {
  // finds running processes matching PID
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const matchingProcesses: Array<Process> = await wrappedFind('pid', processId);
  return matchingProcesses.length > 0 ? matchingProcesses[0] : Promise.reject();
};
export const getProcessName = async (processId: number) =>
  (await getProcessById(processId)).name;
export const getProcessesByName = async (
  processName: string
): Promise<Array<Process>> => {
  // finds running processes matching name
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const matchingProcesses: Array<Process> = await wrappedFind(
    'name',
    processName
  );
  return matchingProcesses;
};
export const getProcess = async (
  processId: number,
  processName: string
): Promise<Process | null | undefined> => {
  try {
    // finds running processes matching PID
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
    const matchingProcesses: Array<Process> = await wrappedFind(
      'pid',
      processId
    );
    // no processes exist with a matching PID
    if (!matchingProcesses.length) return null;
    // Return first matching process if names match
    const previousProcess: Process = matchingProcesses[0];

    if (isObject(previousProcess)) {
      const binPathSegments = previousProcess.bin.split(path.sep);
      const exeName =
        binPathSegments.length > 0
          ? binPathSegments[binPathSegments.length - 1]
          : previousProcess.bin;
      if (exeName === processName || previousProcess.name === processName) {
        return previousProcess;
      }
    }
  } catch (error) {
    logger.error('getProcess error', {
      error,
    });
    return null;
  }
};

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
export const wrappedFind = async (
  type: 'name' | 'pid' | 'port',
  value: string | number | RegExp,
  strict?: boolean
): Promise<
  {
    pid: number;
    ppid?: number;
    uid?: number;
    gid?: number;
    name: string;
    cmd: string;
  }[]
> => {
  const previousLocale = process.env.LC_ALL;
  process.env.LC_ALL = 'C';
  const rv = await find(type, value, strict);
  if (typeof previousLocale !== 'undefined')
    process.env.LC_ALL = previousLocale;
  else delete process.env.LC_ALL;
  return rv;
};
