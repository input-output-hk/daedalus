// @flow
export type SystemInfo = {
  platform: string,
  platformVersion: string,
  cpu: string,
  ram: string,
  availableDiskSpace: string,
  meetsHardwareRequirements: boolean,
  rtsFlagsModeEnabled: boolean,
};
