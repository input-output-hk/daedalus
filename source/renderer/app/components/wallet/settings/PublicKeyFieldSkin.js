'use strict';
// @ts-nocheck
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const Button_1 = require('@react-polymorph/components/Button');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const FormField_1 = require('@react-polymorph/components/FormField');
const props_1 = require('@react-polymorph/utils/props');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/copy.in... Remove this comment to see the full error message
const copy_inline_svg_1 = __importDefault(
  require('../../../assets/images/copy.inline.svg')
);
const PublicKeyField_scss_1 = __importDefault(require('./PublicKeyField.scss'));
function default_1(props) {
  const renderInput = () =>
    react_1.default.createElement('input', {
      ref: props.inputRef,
      ...(0, props_1.pickDOMProps)(props),
      className: (0, classnames_1.default)([
        props.theme[props.themeId].input,
        props.disabled ? props.theme[props.themeId].disabled : null,
        props.error || props.showErrorState
          ? props.theme[props.themeId].errored
          : null,
      ]),
      readOnly: props.readOnly,
      onFocus: () => {
        if (props.onFocus) {
          props.onFocus();
        }
        if (props.inputRef && props.inputRef.current) {
          props.inputRef.current.select();
        }
        props.onCopyValue();
      },
    });
  const render = () =>
    props.valueVisible
      ? react_1.default.createElement(
          PopOver_1.PopOver,
          { content: props.tooltip },
          react_1.default.createElement(
            'div',
            null,
            renderInput(),
            react_1.default.createElement(
              react_copy_to_clipboard_1.default,
              {
                text: props.value,
                onCopy: () => {
                  if (props.inputRef && props.inputRef.current) {
                    props.inputRef.current.select();
                  }
                  props.onCopyValue();
                },
              },
              react_1.default.createElement(Button_1.Button, {
                className: (0, classnames_1.default)([
                  PublicKeyField_scss_1.default.imageButton,
                  PublicKeyField_scss_1.default.copyButton,
                  'flat',
                ]),
                label: react_1.default.createElement(
                  react_svg_inline_1.default,
                  { svg: copy_inline_svg_1.default }
                ),
              })
            )
          )
        )
      : react_1.default.createElement(
          'span',
          null,
          react_1.default.createElement('div', null, renderInput())
        );
  return react_1.default.createElement(FormField_1.FormField, {
    className: props.className,
    disabled: props.disabled,
    label: props.label,
    error: props.error,
    inputRef: props.inputRef,
    theme: props.theme,
    render: render,
  });
}
exports.default = default_1;
//# sourceMappingURL=PublicKeyFieldSkin.js.map
