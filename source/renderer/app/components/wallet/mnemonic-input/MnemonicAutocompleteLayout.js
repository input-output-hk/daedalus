'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.MnemonicAutocompleteLayout = void 0;
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const FormField_1 = require('@react-polymorph/components/FormField');
const Options_1 = require('@react-polymorph/components/Options');
const OptionsSkin_1 = require('@react-polymorph/skins/simple/OptionsSkin');
const MnemonicAutocompleteLayout_scss_1 = __importDefault(
  require('./MnemonicAutocompleteLayout.scss')
);
function MnemonicAutocompleteLayout({
  onClick,
  onChange,
  onSelect,
  onBlur,
  onPaste,
  toggleMouseLocation,
  toggleOpen,
  value,
  ordinalNumber,
  disabled,
  error,
  isOpen,
  optionHeight,
  options,
  optionsMaxHeight,
  noResultsMessage,
  optionsRef,
  rootRef,
  inputRef,
  suggestionsRef,
}) {
  return react_1.default.createElement(
    'div',
    {
      'aria-hidden': true,
      className: MnemonicAutocompleteLayout_scss_1.default.root,
      ref: rootRef,
      role: 'presentation',
    },
    react_1.default.createElement(
      'div',
      { className: MnemonicAutocompleteLayout_scss_1.default.inputLabel },
      ordinalNumber,
      '.'
    ),
    react_1.default.createElement(FormField_1.FormField, {
      error: error,
      disabled: disabled,
      formFieldRef: inputRef,
      isErrorHidden: isOpen,
      render: (setFormFieldRef) =>
        react_1.default.createElement(
          'div',
          {
            ref: suggestionsRef,
            className: MnemonicAutocompleteLayout_scss_1.default.inputWrapper,
          },
          react_1.default.createElement('input', {
            className: (0, classnames_1.default)(
              MnemonicAutocompleteLayout_scss_1.default.input,
              error && MnemonicAutocompleteLayout_scss_1.default.inputError
            ),
            ref: setFormFieldRef,
            value: value,
            onBlur: onBlur,
            onChange: onChange,
            onClick: onClick,
            onPaste: onPaste,
            disabled: disabled,
            spellCheck: 'false',
          })
        ),
    }),
    react_1.default.createElement(Options_1.Options, {
      isOpen: isOpen,
      noResults: !options.length,
      onChange: onSelect,
      options: options,
      persistSearchValue: true,
      optionsRef: optionsRef,
      optionsMaxHeight: optionsMaxHeight,
      resetOnClose: true,
      skin: OptionsSkin_1.OptionsSkin,
      targetRef: suggestionsRef,
      toggleMouseLocation: toggleMouseLocation,
      toggleOpen: toggleOpen,
      optionHeight: optionHeight,
      noResultsMessage: noResultsMessage,
    })
  );
}
exports.MnemonicAutocompleteLayout = MnemonicAutocompleteLayout;
//# sourceMappingURL=MnemonicAutocompleteLayout.js.map
