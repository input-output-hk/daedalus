// @flow
import { observable, action } from 'mobx';
import _ from 'lodash';
import Store from './lib/Store';

export default class UiNotificationsStore extends Store {

  @observable secondsSinceActiveNotificationIsOpen: number = 0;
  @observable maxTimeout: number = 0;
  @observable activeNotificationsList = [];

  _secondsTimerInterval: ?number = null;

  setup() {
    this.actions.notifications.open.listen(this._onOpen);
    this.actions.notifications.closeActiveNotification.listen(this._onClose);
  }

  countdownSinceNotificationOpened = (countDownTo: number) => {
    this.maxTimeout = countDownTo;
    Math.max(countDownTo - this.secondsSinceActiveNotificationIsOpen, 0);
  };

  @action _onOpen = ({ notification } : { notification : Function }) => {
    this.secondsSinceActiveNotificationIsOpen = 0;

    if (this._secondsTimerInterval) clearInterval(this._secondsTimerInterval);
    if (this.maxTimeout > 0) {
      this._secondsTimerInterval = setInterval(this._updateSeconds, 1000);
    }

    this.activeNotificationsList.push({
      key: this.activeNotificationsList.length,
      notification,
      remaining: this.maxTimeout,
    });
  };

  @action _resetActiveNotificationsListOnTimeout = () => {
    let counter = 0;
    const newActiveList = [];
    for (let i = this.activeNotificationsList.length - 1; i >= 0; i--) {
      const activeNotification = {};
      if (this.activeNotificationsList[i].remaining > 0) {
        activeNotification.notification = this.activeNotificationsList[i].notification;
        activeNotification.remaining = this.activeNotificationsList[i].remaining - 1;
        activeNotification.key = counter;
        counter++;
        newActiveList.push(activeNotification);
      }
    }

    this.activeNotificationsList = newActiveList;
    if (newActiveList.length === 0) {
      if (this._secondsTimerInterval) clearInterval(this._secondsTimerInterval);
    }
  };

  @action _onClose = ({ key } : { key : number }) => {
    const index = _.findIndex(this.activeNotificationsList, { key });
    this.activeNotificationsList = this.activeNotificationsList.splice(index, 1);
  };

  @action _updateSeconds = () => {
    this.secondsSinceActiveNotificationIsOpen += 1;
    this._resetActiveNotificationsListOnTimeout();
  };

}
