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
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
// @ts-nocheck
const react_1 = __importStar(require('react'));
const NumericInput_1 = require('@react-polymorph/components/NumericInput');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const API_1 = require('@react-polymorph/themes/API');
const TinyInput_scss_1 = __importDefault(require('./TinyInput.scss'));
class TinyInput extends react_1.Component {
  state = {
    isEditMode: false,
  };
  setEditMode = (isEditMode) =>
    this.setState({
      isEditMode,
    });
  onKeyPress = (evt) => {
    const { onKeyPress, onSubmit } = this.props;
    const { charCode } = evt;
    // @ts-ignore ts-migrate(2559) FIXME: Type 'EventTarget' has no properties in common wit... Remove this comment to see the full error message
    const control = evt.target;
    if (onKeyPress) {
      onKeyPress(evt);
    }
    if (charCode === 13 && control.blur) {
      control.blur();
      if (onSubmit) {
        onSubmit();
      }
    }
  };
  render() {
    const {
      autoFocus,
      innerLabelPrefix,
      innerLabelSuffix,
      innerValue,
      useReadMode,
      type,
      ...restProps
    } = this.props;
    const { isEditMode } = this.state;
    return react_1.default.createElement(
      'div',
      {
        className: TinyInput_scss_1.default.component,
        onFocus: () => this.setEditMode(true),
        onBlur: () => this.setEditMode(false),
        role: 'button',
        tabIndex: -1,
      },
      useReadMode &&
        !isEditMode &&
        react_1.default.createElement(
          'div',
          { className: TinyInput_scss_1.default.contentInReadMode },
          react_1.default.createElement(
            'span',
            { className: TinyInput_scss_1.default.innerLabelPrefix },
            innerLabelPrefix
          ),
          react_1.default.createElement(
            'span',
            { className: TinyInput_scss_1.default.innerValue },
            innerValue
          ),
          react_1.default.createElement(
            'span',
            { className: TinyInput_scss_1.default.innerLabelSuffix },
            innerLabelSuffix
          )
        ),
      (!useReadMode || isEditMode) &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          type === 'number'
            ? react_1.default.createElement(NumericInput_1.NumericInput, {
                themeId: API_1.IDENTIFIERS.INPUT,
                skin: InputSkin_1.InputSkin,
                ...restProps,
                autoFocus: autoFocus,
                onKeyPress: this.onKeyPress,
              })
            : react_1.default.createElement(Input_1.Input, {
                themeId: API_1.IDENTIFIERS.INPUT,
                skin: InputSkin_1.InputSkin,
                ...restProps,
                autoFocus: useReadMode ? true : autoFocus,
                onKeyPress: this.onKeyPress,
                type: type,
              })
        )
    );
  }
}
exports.default = TinyInput;
//# sourceMappingURL=TinyInput.js.map
