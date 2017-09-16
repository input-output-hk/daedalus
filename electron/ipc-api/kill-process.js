import { ipcMain } from 'electron';

export default () => {
  ipcMain.on('kill-process', () => {
    // if (event.sender !== mainWindow.webContents) return;
    // TODO: fix this
    process.exit(20);
  });
};
