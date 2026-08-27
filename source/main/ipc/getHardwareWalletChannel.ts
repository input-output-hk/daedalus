import type { BrowserWindow } from 'electron';

import {
  hardwareWalletService,
  HardwareWalletService,
} from '../hardware/HardwareWalletService';
import type { HardwareWalletChannels } from './createHardwareWalletIPCChannels';

export { hardwareWalletService };

export const handleHardwareWalletRequests = async (
  _mainWindow: BrowserWindow,
  channels: HardwareWalletChannels,
  service: HardwareWalletService = hardwareWalletService
): Promise<void> => service.register(channels);
