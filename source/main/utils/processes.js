import psList from 'ps-list';

export type Process = {
  pid: number,
  name: string,
};

export const getProcessName = async (processId: number): Promise<string> => {
  // retrieves all running processes
  const processes: Array<Process> = await psList();
  // filters running processes against previous PID
  const matches: Array<Process> = processes.filter(({ pid }) => processId === pid);
  return matches.length > 0 ? matches[0].name : Promise.reject();
};
