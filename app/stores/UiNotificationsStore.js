// @flow
import { observable, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';

export default class UiNotificationsStore extends Store {

  @observable activeNotifications = [];

  setup() {
    this.actions.notifications.open.listen(this._onOpen);
    this.actions.notifications.closeActiveNotification.listen(this._onClose);
  }

  isOpen = (id: string): boolean => _.size(_.find(this.activeNotifications, ['id', id]));

  @action _onOpen = ({ id, duration } : { id : string, duration: number }) => {
    const notification = {};
    notification.id = id;
    if (duration) {
      notification.duration = duration;
      notification.secondsTimerInterval = setInterval(this._updateSeconds, 1000, id);
    }

    if (this.isOpen(id)) {
      this._onClose(id);
      setTimeout(() => this._set(notification), 200);
    } else {
      this._set(notification);
    }
  };

  @action _set = (notification: Object) => {
    this.activeNotifications.push(notification);
  };

  @action _onClose = (id: string) => {
    const currentNotification = _.find(this.activeNotifications, ['id', id]);
    if (currentNotification) {
      if (currentNotification.secondsTimerInterval) {
        clearInterval(currentNotification.secondsTimerInterval);
      }
      this.activeNotifications = _.reject(this.activeNotifications, ['id', id]);
    }
  };

  @action _resetAll = () => {
    _.map(this.activeNotifications, (notification) => {
      this._onClose(notification.id);
    });
  };

  @action _updateSeconds = (id: string) => {
    const currentNotification = _.find(this.activeNotifications, ['id', id]);
    const duration = currentNotification.duration - 1;
    if (duration === 0) {
      this._onClose(id);
    } else {
      currentNotification.duration -= 1;
      this._set(currentNotification);
    }
  };
}
