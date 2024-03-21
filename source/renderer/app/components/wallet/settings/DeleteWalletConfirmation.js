'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const form_1 = require('../../../utils/form');
const DeleteWalletConfirmationDialog_scss_1 = __importDefault(
  require('./DeleteWalletConfirmationDialog.scss')
);
function DeleteWalletConfirmation({
  isBackupNoticeAccepted,
  confirmationValue,
  onAcceptBackupNotice,
  onConfirmationValueChange,
  handleSubmit,
  checkboxLabel,
  inputLabel,
}) {
  return react_1.default.createElement(
    react_1.default.Fragment,
    null,
    react_1.default.createElement(Checkbox_1.Checkbox, {
      label: checkboxLabel,
      onChange: onAcceptBackupNotice,
      checked: isBackupNoticeAccepted,
      skin: CheckboxSkin_1.CheckboxSkin,
    }),
    isBackupNoticeAccepted &&
      react_1.default.createElement(Input_1.Input, {
        className:
          DeleteWalletConfirmationDialog_scss_1.default.confirmationInput,
        label: inputLabel,
        value: confirmationValue,
        onKeyPress: (event) => (0, form_1.submitOnEnter)(handleSubmit, event),
        onChange: onConfirmationValueChange,
        skin: InputSkin_1.InputSkin,
      })
  );
}
exports.default = DeleteWalletConfirmation;
//# sourceMappingURL=DeleteWalletConfirmation.js.map
