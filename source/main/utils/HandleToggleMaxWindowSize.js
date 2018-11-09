export default (mainWindow) => {
  let currentWindowBounds = mainWindow.webContents.getOwnerBrowserWindow().getBounds();
  let isMaximized = false;
  let isToggling = false;

  mainWindow.on('resize', () => {
    if (isToggling) return;
    isMaximized = false;
    currentWindowBounds = mainWindow.webContents.getOwnerBrowserWindow().getBounds();
  });

  return () => {
    isToggling = true;
    if (isMaximized) {
      mainWindow.webContents.getOwnerBrowserWindow().setBounds(currentWindowBounds, true);
      isMaximized = false;
    } else {
      mainWindow.maximize();
      isMaximized = true;
    }
    isToggling = false;
  };
};
