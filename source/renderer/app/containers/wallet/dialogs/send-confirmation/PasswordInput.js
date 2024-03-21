'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.PasswordInput = void 0;
// @ts-nocheck
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const helpers_1 = require('./helpers');
const HardwareWalletStatus_1 = __importDefault(
  require('../../../../components/hardware-wallet/HardwareWalletStatus')
);
const styles_scss_1 = __importDefault(require('./styles.scss'));
function Component({
  isFlight,
  isTrezor,
  isHardwareWallet,
  areTermsAccepted,
  passphraseField,
  walletName,
  hwDeviceStatus,
  onExternalLinkClick,
  handleSubmitOnEnter,
}) {
  if ((0, helpers_1.doTermsNeedAcceptance)({ isFlight, areTermsAccepted })) {
    return null;
  }
  return isHardwareWallet
    ? react_1.default.createElement(
        'div',
        { className: styles_scss_1.default.hardwareWalletStatusWrapper },
        react_1.default.createElement(HardwareWalletStatus_1.default, {
          hwDeviceStatus: hwDeviceStatus,
          walletName: walletName,
          isTrezor: isTrezor,
          onExternalLinkClick: onExternalLinkClick,
        })
      )
    : react_1.default.createElement(Input_1.Input, {
        type: 'password',
        className: styles_scss_1.default.passphrase,
        ...passphraseField.bind(),
        error: passphraseField.error,
        skin: InputSkin_1.InputSkin,
        onKeyPress: handleSubmitOnEnter,
        autoFocus: true,
      });
}
exports.PasswordInput = (0, mobx_react_1.observer)(Component);
//# sourceMappingURL=PasswordInput.js.map
