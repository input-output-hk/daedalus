// @flow
import Action from '../actions/lib/Action';
import type { NotificationMessageProps } from '../components/notifications/Notification.js';

export type NotificationConfig = {
  id: string,
  duration?: number,
  actionToListenAndOpen: Action<any>,
  actionToListenAndClose?: Action<any>,
};

export type StoredNotification = {
  notificationConfig: NotificationConfig,
  notificationMessage: NotificationMessageProps,
  labelValues?: Object,
};
