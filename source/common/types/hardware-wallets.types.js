'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.DeviceEvents = exports.DeviceTypes = exports.DeviceModels = void 0;
exports.DeviceModels = {
  LEDGER_NANO_S: 'nanoS',
  LEDGER_NANO_S_PLUS: 'nanoSP',
  LEDGER_NANO_X: 'nanoX',
  TREZOR_ONE: '1',
  TREZOR_T: 'T',
};
var DeviceTypes;
(function (DeviceTypes) {
  DeviceTypes['LEDGER'] = 'ledger';
  DeviceTypes['TREZOR'] = 'trezor';
})((DeviceTypes = exports.DeviceTypes || (exports.DeviceTypes = {})));
exports.DeviceEvents = {
  CONNECT: 'device-connect',
  CONNECT_UNACQUIRED: 'device-connect_unacquired',
  DISCONNECT: 'device-disconnect',
  CHANGED: 'device-changed',
  ACQUIRE: 'device-acquire',
  RELEASE: 'device-release',
  ACQUIRED: 'device-acquired',
  RELEASED: 'device-released',
  USED_ELSEWHERE: 'device-used_elsewhere',
  LOADING: 'device-loading',
  BUTTON: 'button',
  PIN: 'pin',
  PASSPHRASE: 'passphrase',
  PASSPHRASE_ON_DEVICE: 'passphrase_on_device',
  WORD: 'word',
  WAIT_FOR_SELECTION: 'device-wait_for_selection',
  UNREADABLE: 'unreadable-device',
};
//# sourceMappingURL=hardware-wallets.types.js.map
