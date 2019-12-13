// @flow
import { observable, action } from 'mobx';
import { /* set, */ omit } from 'lodash';
import Store from './lib/Store';
import type { StoredNotification } from '../types/notificationType';

export default class UiNotificationsStore extends Store {
  @observable activeNotifications: {} = {};
  @observable notifications: {} = {};

  setup() {
    this.actions.notifications.registerNotification.listen(
      this._registerNotification
    );
    this.actions.notifications.closeNotification.listen(this._onClose);
  }

  isOpen = (id: string): boolean => !!this.activeNotifications[id];

  @action _registerNotification = (notification: StoredNotification) => {
    const { id } = notification.config;
    this.notifications[id] = notification;
    const { actionToListenAndOpen } = notification.config;
    actionToListenAndOpen.listen((labelValues?: Object) =>
      this._openNotification(notification, labelValues)
    );
  };

  @action _openNotification = (
    notification: StoredNotification,
    labelValues?: Object
  ) => {
    const { id, duration = 5000 } = notification.config;
    this.activeNotifications[id] = { labelValues };
    setTimeout(() => this._onClose({ id }), duration);
  };

  @action _onClose = ({ id }: { id: string }) => {
    const notification = this.activeNotifications[id];
    if (notification) {
      this.activeNotifications = omit(this.activeNotifications, id);
    }
  };

  @action _updateSeconds = (id: string) => {
    const notification = this.activeNotifications[id];
    if (notification && notification.duration) {
      notification.duration -= 1;
      if (notification.duration === 0) this._onClose({ id });
    }
  };
}
