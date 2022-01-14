// @flow
export type Cpu = {
  model: string,
  speed: number,
};

export type SystemInfo = {
  platform: string,
  platformVersion: string,
  cpu: Cpu,
  ram: string,
  availableDiskSpace: string,
};
