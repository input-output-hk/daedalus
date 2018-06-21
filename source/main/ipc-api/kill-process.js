// @flow
import { ipcMain } from 'electron';

export default () => {
  ipcMain.on('kill-process', () => {
    // if (event.sender !== mainWindow.webContents) return;
    // TODO: fix this (are we ensuring that this message is only allowed from mainWindow?)
    process.exit(20);
  });
};
