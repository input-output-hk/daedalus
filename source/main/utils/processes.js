// @flow
import psList from 'ps-list';
import { isObject } from 'lodash';
import { logger } from './logging';

/* eslint-disable consistent-return */

export type Process = {
  pid: number,
  name: string,
  cmd: string,
  ppid?: number,
  cpu: number,
  memory: number,
};

export const getProcessById = async (processId: number): Promise<Process> => {
  // retrieves all running processes
  const processes: Array<Process> = await psList();
  // filters running processes against previous PID
  const matches: Array<Process> = processes.filter(
    ({ pid }) => processId === pid
  );
  return matches.length > 0 ? matches[0] : Promise.reject();
};

export const getProcessName = async (processId: number) =>
  (await getProcessById(processId)).name;

export const getProcessesByName = async (
  processName: string
): Promise<Array<Process>> => {
  // retrieves all running processes
  const processes: Array<Process> = await psList();
  // filters running processes against previous PID
  return processes.filter(({ name }) => processName === name);
};

export const getProcess = async (
  processId: number,
  processName: string
): Promise<?Process> => {
  try {
    // retrieves all running processes
    const runningProcesses: Array<Process> = await psList();
    // filters running processes against given pid
    const matchingProcesses: Array<Process> = runningProcesses.filter(
      ({ pid }) => pid === processId
    );
    // no processes exist with a matching PID
    if (!matchingProcesses.length) return null;
    // Return first matching process if names match
    const previousProcess: Process = matchingProcesses[0];
    if (isObject(previousProcess) && previousProcess.name === processName) {
      return previousProcess;
    }
  } catch (error) {
    logger.error('getProcess error', { error });
    return null;
  }
};
