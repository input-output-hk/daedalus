export const waitForActiveImportNotification = (client, { isHidden } = {}) => (
  client.waitForVisible('.ActiveImportNotification', null, isHidden)
);
