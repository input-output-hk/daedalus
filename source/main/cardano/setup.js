// @flow
import { BrowserWindow } from 'electron';
import { CardanoNode } from './CardanoNode';
import { frontendOnlyMode } from '../config';
import type { LauncherConfig } from '../config';
import { setupFrontendOnlyMode } from './setupFrontendOnlyMode';
import { setupCardanoNodeMode } from './setupCardanoNodeMode';

/**
 * Configures, starts and manages the CardanoNode responding to node
 * state changes, app events and IPC messages coming from the renderer.
 *
 * @param launcherConfig {LauncherConfig}
 * @param mainWindow
 */
export const setupCardano = (
  launcherConfig: LauncherConfig, mainWindow: BrowserWindow
): ?CardanoNode => {
  if (frontendOnlyMode) {
    return setupFrontendOnlyMode(mainWindow);
  }
  return setupCardanoNodeMode(launcherConfig, mainWindow);
};
