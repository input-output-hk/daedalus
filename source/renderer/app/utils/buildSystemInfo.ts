import type { SystemInfo } from '../types/systemInfoTypes';
import { formattedBytesToSize } from './formatters';
import type { Environment } from '../../../common/types/environment.types';
import NetworkStatusStore from '../stores/NetworkStatusStore';

export const buildSystemInfo = (
  environment: Environment,
  networkStatus: NetworkStatusStore
): SystemInfo => ({
  platform: environment.os,
  platformVersion: environment.platformVersion,
  cpu: Array.isArray(environment.cpu) ? environment.cpu[0].model : '',
  ram: formattedBytesToSize(environment.ram),
  hasMetHardwareRequirements: environment.hasMetHardwareRequirements,
  availableDiskSpace: networkStatus.diskSpaceAvailable,
  isRTSFlagsModeEnabled: networkStatus.isRTSFlagsModeEnabled,
});
