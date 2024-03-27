'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleFormErrors = void 0;
const mobx_react_form_1 = __importDefault(require('mobx-react-form'));
const waitForExist_1 = require('./waitForExist');
class ReactToolboxMobxForm extends mobx_react_form_1.default {
  bindings() {
    return {
      ReactToolbox: {
        id: 'id',
        name: 'name',
        type: 'type',
        value: 'value',
        label: 'label',
        placeholder: 'hint',
        disabled: 'disabled',
        error: 'error',
        onChange: 'onChange',
        onFocus: 'onFocus',
        onBlur: 'onBlur',
      },
    };
  }
}
exports.default = ReactToolboxMobxForm;
const handleFormErrors = async (querySelector, options = {}) => {
  const { focusElement } = options;
  const firstErrorLabel = await (0, waitForExist_1.waitForExist)(querySelector);
  if (firstErrorLabel) {
    firstErrorLabel.scrollIntoView({
      behavior: 'smooth',
    });
  }
  if (
    focusElement &&
    firstErrorLabel &&
    firstErrorLabel.parentNode instanceof HTMLElement
  ) {
    const input = firstErrorLabel.parentNode.querySelector(
      'div:not(.SimpleAutocomplete_selectedWords) > input'
    );
    if (input) return setTimeout(() => input.focus(), 500);
  }
  return false;
};
exports.handleFormErrors = handleFormErrors;
//# sourceMappingURL=ReactToolboxMobxForm.js.map
