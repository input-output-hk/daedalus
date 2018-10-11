import psList from 'ps-list';

export type Process = {
  pid: number,
  name: string,
  cmd: string,
  ppid?: number,
  cpu: number,
  memore: number,
};

export const getProcessById = async (processId: number): Promise<Process> => {
  // retrieves all running processes
  const processes: Array<Process> = await psList();
  // filters running processes against previous PID
  const matches: Array<Process> = processes.filter(({ pid }) => processId === pid);
  return matches.length > 0 ? matches[0] : Promise.reject();
};

export const getProcessName = async (processId: number) => (
  (await getProcessById(processId)).name
);

export const getProcessesByName = async (processName: string): Promise<Array<Process>> => {
  // retrieves all running processes
  const processes: Array<Process> = await psList();
  // filters running processes against previous PID
  return processes.filter(({ name }) => processName === name);
};
