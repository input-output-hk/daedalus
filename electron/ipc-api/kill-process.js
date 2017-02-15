import { ipcMain } from 'electron';

export default ({ mainWindow }) => {
  ipcMain.on('kill-process', (event) => {
    // if (event.sender !== mainWindow.webContents) return;
    // TODO: fix this
    process.exit(20);
  });
};
