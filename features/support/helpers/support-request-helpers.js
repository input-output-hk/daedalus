
export const resetSupportWindow = async (context) => {
  context.client.frameParent();
  const windows = await context.client.getTabIds();
  if (windows.length === 1) return;
  const currentWindow = await context.client.getCurrentTabId();
  if (windows[1] === currentWindow) {
    context.client.close();
  }
  await switchToWindow(context, 0);
};

export const switchToWindow = async (context, index) => {
  let windows;
  await context.client.waitUntil(async () => {
    windows = await context.client.getTabIds();
    return windows.length > index;
  });
  context.client.switchTab(windows[index]);
  return windows;
};

export const getFieldValue = async (context, selects, index) => {
  const fieldText = await context.client.elementIdText(selects.value[index].ELEMENT);
  return fieldText.value;
};

export const fieldHasValue = (value) => value && value !== '-';
