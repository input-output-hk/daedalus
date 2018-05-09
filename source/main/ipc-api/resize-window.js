// @flow
import { ipcMain } from 'electron';

export default ({ window }: { window: any }) => {
  ipcMain.on('resize-window', (event, { width, height, animate }) => {
    if (event.sender !== window.webContents) return;
    window.setSize(width, height, animate);
  });
};
