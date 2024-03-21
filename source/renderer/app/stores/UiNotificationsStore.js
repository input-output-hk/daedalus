'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const timingConfig_1 = require('../config/timingConfig');
class UiNotificationsStore extends Store_1.default {
  // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  activeNotifications = {};
  // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
  activeNotificationsTimeouts = {};
  setup() {
    this.actions.notifications.registerNotification.listen(
      this._registerNotification
    );
    this.actions.notifications.closeNotification.listen(this._onClose);
  }
  isOpen = (id) => !!this.activeNotifications[id];
  _registerNotification = (notificationConfig) => {
    const {
      id,
      actionToListenAndOpen,
      actionToListenAndClose,
    } = notificationConfig;
    actionToListenAndOpen.listen((labelValues) =>
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
  _openNotification = (notificationConfig, labelValues) => {
    const {
      id,
      duration = timingConfig_1.NOTIFICATION_DEFAULT_DURATION,
    } = notificationConfig;
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
  _onClose = ({ id }) => {
    if (id in this.activeNotifications) {
      // @ts-ignore ts-migrate(2740) FIXME: Type 'Pick<Record<NotificationId, { labelValues?: ... Remove this comment to see the full error message
      this.activeNotifications = (0, lodash_1.omit)(
        this.activeNotifications,
        id
      );
    }
  };
}
__decorate(
  [
    mobx_1.observable,
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    __metadata('design:type', Object),
  ],
  UiNotificationsStore.prototype,
  'activeNotifications',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiNotificationsStore.prototype,
  '_registerNotification',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiNotificationsStore.prototype,
  '_openNotification',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  UiNotificationsStore.prototype,
  '_onClose',
  void 0
);
exports.default = UiNotificationsStore;
//# sourceMappingURL=UiNotificationsStore.js.map
