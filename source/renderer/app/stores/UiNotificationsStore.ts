import { observable, action } from 'mobx';
import { omit } from 'lodash';
import Store from './lib/Store';
import { NOTIFICATION_DEFAULT_DURATION } from '../config/timingConfig';
import type {
  NotificationConfig,
  NotificationId,
} from '../types/notificationTypes';

export default class UiNotificationsStore extends Store {
  @observable
  // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  activeNotifications: Record<
    NotificationId,
    {
      labelValues?: {};
      index: number;
    }
  > = {};
  // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  activeNotificationsTimeouts: Record<NotificationId, TimeoutID> = {};

  setup() {
    this.actions.notifications.registerNotification.listen(
      this._registerNotification
    );
    this.actions.notifications.closeNotification.listen(this._onClose);
  }

  isOpen = (id: NotificationId): boolean => !!this.activeNotifications[id];
  @action
  _registerNotification = (notificationConfig: NotificationConfig) => {
    const {
      id,
      actionToListenAndOpen,
      actionToListenAndClose,
    } = notificationConfig;
    actionToListenAndOpen.listen((labelValues?: Record<string, any>) =>
      this._openNotification(notificationConfig, labelValues)
    );

    if (actionToListenAndClose) {
      actionToListenAndClose.listen(() =>
        this._onClose({
          id,
        })
      );
    }
  };
  @action
  _openNotification = (
    notificationConfig: NotificationConfig,
    labelValues?: Record<string, any>
  ) => {
    const { id, duration = NOTIFICATION_DEFAULT_DURATION } = notificationConfig;
    const index = Object.keys(this.activeNotifications).length + 1;
    this.activeNotifications[id] = {
      labelValues,
      index,
    };
    clearTimeout(this.activeNotificationsTimeouts[id]);
    this.activeNotificationsTimeouts[id] = setTimeout(
      () =>
        this._onClose({
          id,
        }),
      duration
    );
  };
  @action
  _onClose = ({ id }: { id: NotificationId }) => {
    if (id in this.activeNotifications) {
      // @ts-ignore ts-migrate(2740) FIXME: Type 'Pick<Record<NotificationId, { labelValues?: ... Remove this comment to see the full error message
      this.activeNotifications = omit(this.activeNotifications, id);
    }
  };
}
