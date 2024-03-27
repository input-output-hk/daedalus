'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const WalletTokenPicker_messages_1 = require('./WalletTokenPicker.messages');
const const_1 = require('./const');
function WalletTokenPickerCheckbox({
  intl,
  className,
  isChecked,
  isMaxCount,
  isPreviouslyChecked,
  uniqueId,
  toggleCheckbox,
}) {
  const checked = isChecked || isPreviouslyChecked;
  const toolTipDisabled = !isMaxCount || checked;
  const onChange = (0, react_1.useCallback)(() => toggleCheckbox(uniqueId), [
    toggleCheckbox,
    uniqueId,
  ]);
  return react_1.default.createElement(
    'div',
    { className: className },
    react_1.default.createElement(
      PopOver_1.PopOver,
      {
        maxWidth: 315,
        disabled: toolTipDisabled,
        content: intl.formatMessage(
          WalletTokenPicker_messages_1.messages.maxTokensWarning,
          {
            maxTokens: const_1.MAX_TOKENS,
          }
        ),
      },
      react_1.default.createElement(Checkbox_1.Checkbox, {
        checked: checked,
        onChange: onChange,
        disabled: isPreviouslyChecked,
      })
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(WalletTokenPickerCheckbox);
//# sourceMappingURL=WalletTokenPickerCheckbox.js.map
