import find from 'find-process';
import { isObject } from 'lodash';
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
  const matchingProcesses: Array<Process> = await find('pid', processId);
  return matchingProcesses.length > 0 ? matchingProcesses[0] : Promise.reject();
};
export const getProcessName = async (processId: number) =>
  (await getProcessById(processId)).name;
export const getProcessesByName = async (
  processName: string
): Promise<Array<Process>> => {
  // finds running processes matching name
  // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
  const matchingProcesses: Array<Process> = await find('name', processName);
  return matchingProcesses;
};
export const getProcess = async (
  processId: number,
  processName: string
): Promise<Process | null | undefined> => {
  try {
    // finds running processes matching PID
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ pid: number; ppid?: number; uid?: number; ... Remove this comment to see the full error message
    const matchingProcesses: Array<Process> = await find('pid', processId);
    // no processes exist with a matching PID
    if (!matchingProcesses.length) return null;
    // Return first matching process if names match
    const previousProcess: Process = matchingProcesses[0];

    if (isObject(previousProcess) && previousProcess.name === processName) {
      return previousProcess;
    }
  } catch (error) {
    logger.error('getProcess error', {
      error,
    });
    return null;
  }
};
