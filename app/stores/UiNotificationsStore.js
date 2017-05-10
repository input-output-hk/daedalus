// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';
import type { Notification } from '../types/notificationType';

export default class UiNotificationsStore extends Store {

  @observable activeNotifications: Array<Notification> = [];

  setup() {
    this.actions.notifications.open.listen(this._onOpen);
    this.actions.notifications.closeActiveNotification.listen(this._onClose);
  }

  isOpen = (id: string): boolean => !!this._findNotificationById(id);

  _findNotificationById = (id: string): ?Notification =>
    this.activeNotifications.find(notification => notification.id === id);

  @action _onOpen = ({ id, duration } : { id: string, duration?: number }) => {
    const notification = {
      id,
      duration: duration || null,
      secondsTimerInterval: duration ? setInterval(this._updateSeconds, 1000, id) : null,
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
    this.activeNotifications.push(notification);
  };

  @action _onClose = ({ id } : { id: string }) => {
    const notification = this._findNotificationById(id);
    if (notification) {
      if (notification.secondsTimerInterval) clearInterval(notification.secondsTimerInterval);
      const indexOfNotification = this.activeNotifications.indexOf(notification);
      this.activeNotifications.splice(indexOfNotification, 1);
    }
  };

  @action _updateSeconds = (id: string) => {
    const notification = this._findNotificationById(id);
    if (notification && notification.duration) {
      notification.duration -= 1;
      if (notification.duration === 0) this._onClose({ id });
    }
  };
}
