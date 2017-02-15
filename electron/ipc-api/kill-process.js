import { ipcMain } from 'electron';

export default ({ mainWindow }) => {
  ipcMain.on('kill-process', (event) => {
    if (event.sender !== mainWindow.webContents) return;
    process.exit(20);
  });
};
