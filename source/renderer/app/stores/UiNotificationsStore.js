// @flow
import { observable, action } from 'mobx';
import { omit } from 'lodash';
import Store from './lib/Store';
import { NOTIFICATION_DEFAULT_DURATION } from '../config/timingConfig';
import type {
  NotificationConfig,
  NotificationId,
} from '../types/notificationTypes';

export default class UiNotificationsStore extends Store {
  @observable activeNotifications: {
    [key: NotificationId]: {
      labelValues?: {},
    },
  } = {};

  setup() {
    this.actions.notifications.registerNotification.listen(
      this._registerNotification
    );
    this.actions.notifications.closeNotification.listen(this._onClose);
  }

  isOpen = (id: NotificationId): boolean => !!this.activeNotifications[id];

  @action _registerNotification = (notificationConfig: NotificationConfig) => {
    const {
      id,
      actionToListenAndOpen,
      actionToListenAndClose,
    } = notificationConfig;
    actionToListenAndOpen.listen((labelValues?: Object) =>
      this._openNotification(notificationConfig, labelValues)
    );
    if (actionToListenAndClose) {
      actionToListenAndClose.listen(() => this._onClose({ id }));
    }
  };

  @action _openNotification = (
    notificationConfig: NotificationConfig,
    labelValues?: Object
  ) => {
    const { id, duration = NOTIFICATION_DEFAULT_DURATION } = notificationConfig;
    this.activeNotifications[id] = { labelValues };
    setTimeout(() => this._onClose({ id }), duration);
  };

  @action _onClose = ({ id }: { id: NotificationId }) => {
    if (id in this.activeNotifications) {
      this.activeNotifications = omit(this.activeNotifications, id);
    }
  };
}
