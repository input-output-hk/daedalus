import { ipcMain } from 'electron';
import type { BrowserWindow } from 'electron';

export default (window: BrowserWindow) => {
  ipcMain.on('resize-window', (event, { width, height, animate }) => {
    if (event.sender !== window.webContents) return;
    window.setSize(width, height, animate);
  });
};
