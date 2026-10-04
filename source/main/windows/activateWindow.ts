import { app } from 'electron';
import type { BrowserWindow } from 'electron';

/**
 * Bring the main window to the front for a second launch of Daedalus:
 * restore it if minimised, show it if hidden, and focus it.
 *
 * On macOS a launch with `open -n` leaves the new, windowless process as the
 * active application, so Daedalus has to take activation back explicitly.
 */
export const activateWindow = (
  window: BrowserWindow | null | undefined,
  platform: NodeJS.Platform = process.platform
): boolean => {
  if (!window || window.isDestroyed()) return false;
  if (window.isMinimized()) window.restore();
  if (!window.isVisible()) window.show();
  if (platform === 'darwin') app.focus({ steal: true });
  window.focus();
  return true;
};
