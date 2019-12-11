// @flow
import { observable, action } from 'mobx';
import { /* set, */ omit } from 'lodash';
import Store from './lib/Store';
import type { StoredNotification } from '../types/notificationType';

export default class UiNotificationsStore extends Store {
  @observable activeNotifications: {} = {};

  setup() {
    this.actions.notifications.registerNotification.listen(
      this._registerNotification
    );
    this.actions.notifications.closeActiveNotification.listen(this._onClose);
  }

  isOpen = (id: string): boolean => !!this.activeNotifications[id];

  @action _registerNotification = (storedNotification: StoredNotification) => {
    const { actionToListenAndOpen } = storedNotification.notificationConfig;
    actionToListenAndOpen.listen((labelValues?: Object) =>
      this._openNotification(storedNotification, labelValues)
    );
  };

  @action _openNotification = (
    storedNotification: StoredNotification,
    labelValues?: Object
  ) => {
    const { id, duration = 5000 } = storedNotification.notificationConfig;
    this.activeNotifications[id] = { ...storedNotification, labelValues };
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
