'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const RemoveWalletButton_scss_1 = __importDefault(
  require('./RemoveWalletButton.scss')
);
function WalletSettingsRemoveButton({ onClick, label, disabled = false }) {
  return react_1.default.createElement(Button_1.Button, {
    className: 'flat',
    disabled: disabled,
    label: label,
    onClick: onClick,
    skin: ButtonSkin_1.ButtonSkin,
    themeOverrides: RemoveWalletButton_scss_1.default,
  });
}
exports.default = WalletSettingsRemoveButton;
//# sourceMappingURL=WalletSettingsRemoveButton.js.map
