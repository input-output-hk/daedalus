import { ipcMain } from 'electron';

export default ({ mainWindow }) => {
  ipcMain.on('resize-window', (event, { width, height, animate }) => {
    if (event.sender !== mainWindow.webContents) return;
    mainWindow.setSize(width, height, animate);
  });
};
