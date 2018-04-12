export const waitForActiveRestoreNotification = (client, { isHidden } = {}) => (
  client.waitForVisible('.ActiveRestoreNotification', null, isHidden)
);
