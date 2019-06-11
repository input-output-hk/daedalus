// @flow
import { observable, action } from 'mobx';
import { set, omit } from 'lodash';
import Store from './lib/Store';
import type { Notification } from '../types/notificationType';

export default class UiNotificationsStore extends Store {
  @observable activeNotifications: {} = {};

  setup() {
    this.actions.notifications.open.listen(this._onOpen);
    this.actions.notifications.closeActiveNotification.listen(this._onClose);
  }

  isOpen = (id: string): boolean => !!this.activeNotifications[id];

  @action _onOpen = ({ id, duration }: { id: string, duration?: number }) => {
    const notification = {
      id,
      duration: duration || null,
      secondsTimerInterval: duration
        ? setInterval(this._updateSeconds, 1000, id)
        : null,
    };

    if (this.isOpen(id)) {
      // if notification is currently active close and reopen it
      this._onClose({ id });
      setTimeout(() => this._set(notification), 200);
    } else {
      this._set(notification);
    }
  };

  @action _set = (notification: Notification) => {
    this.activeNotifications = {
      ...this.activeNotifications,
      ...set({}, notification.id, notification),
    };
  };

  @action _onClose = ({ id }: { id: string }) => {
    const notification = this.activeNotifications[id];
    if (notification) {
      if (notification.secondsTimerInterval)
        clearInterval(notification.secondsTimerInterval);
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
