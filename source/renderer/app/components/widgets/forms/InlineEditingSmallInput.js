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
const mobx_react_1 = require('mobx-react');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const Input_1 = require('@react-polymorph/components/Input');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Button_1 = require('@react-polymorph/components/Button');
const classnames_1 = __importDefault(require('classnames'));
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const InlineEditingSmallInput_scss_1 = __importDefault(
  require('./InlineEditingSmallInput.scss')
);
const timingConfig_1 = require('../../../config/timingConfig');
const pen_inline_svg_1 = __importDefault(
  require('../../../assets/images/pen.inline.svg')
);
const close_cross_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross.inline.svg')
);
const arrow_right_inline_svg_1 = __importDefault(
  require('../../../assets/images/arrow-right.inline.svg')
);
let InlineEditingSmallInput = class InlineEditingSmallInput extends react_1.Component {
  state = {
    isActive: false,
  };
  static defaultProps = {
    onStartEditing: () => {},
    onStopEditing: () => {},
    onCancelEditing: () => {},
  };
  validator = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        inputField: {
          value: this.props.inputFieldValue,
          validators: [
            ({ field }) => [
              this.props.isValid(field.value),
              this.props.validationErrorMessage,
            ],
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.validator.submit({
      onSuccess: (form) => {
        const { inputField } = form.values();
        this.setState({
          isActive: false,
        });
        if (inputField !== this.props.inputFieldValue) {
          this.props.onStopEditing();
          this.props.onSubmit(inputField);
        } else {
          this.props.onCancelEditing();
        }
        this.input.blur();
      },
      onError: (form) => {
        const { inputField } = form.values();
        if (!inputField || !form.isValid) {
          this.setState({
            isActive: false,
          });
          this.props.onSubmit(inputField);
        }
      },
    });
  };
  handleInputKeyDown = (event) => {
    if (event.which === 13) {
      // ENTER key
      this.onBlur();
    }
    if (event.which === 27) {
      // ESCAPE key
      this.onCancel();
    }
  };
  onFocus = () => {
    this.setState({
      isActive: true,
    });
    if (this.props.onStartEditing) this.props.onStartEditing();
  };
  onBlur = () => {
    if (this.state.isActive) {
      this.setState({
        isActive: false,
      });
      this.submit();
    }
  };
  onCancel = () => {
    const inputField = this.validator.$('inputField');
    inputField.value = this.props.inputFieldValue;
    this.setState({
      isActive: false,
    });
    if (this.props.onCancelEditing) this.props.onCancelEditing();
    this.input.blur();
  };
  componentDidUpdate() {
    if (this.props.isActive) {
      const { inputBlocked } = this.props;
      // eslint-disable-next-line no-unused-expressions
      this.inputField && !inputBlocked && this.inputField.focus();
    }
  }
  get input() {
    const fallbackInput = {
      blur: () => {},
      focus: () => {},
    };
    return this?.inputField?.inputElement?.current || fallbackInput;
  }
  inputField;
  render() {
    const { validator } = this;
    const {
      className,
      inputFieldLabel,
      isDisabled,
      inputBlocked,
      maxLength,
      placeholder,
    } = this.props;
    const { isActive } = this.state;
    let { successfullyUpdated } = this.props;
    const inputField = validator.$('inputField');
    const arrowIconIsVisible = inputField.value !== this.props.inputFieldValue;
    const componentStyles = (0, classnames_1.default)([
      className,
      InlineEditingSmallInput_scss_1.default.component,
      isActive ? InlineEditingSmallInput_scss_1.default.isActive : null,
      isDisabled ? InlineEditingSmallInput_scss_1.default.disabled : null,
      inputField.error ? InlineEditingSmallInput_scss_1.default.hasError : null,
      !arrowIconIsVisible
        ? InlineEditingSmallInput_scss_1.default.withoutRightButton
        : null,
    ]);
    if (isActive || inputBlocked) {
      successfullyUpdated = false;
    }
    const inputStyles = (0, classnames_1.default)([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer',
    ]);
    const leftButtonStyles = (0, classnames_1.default)([
      InlineEditingSmallInput_scss_1.default.leftButton,
      !arrowIconIsVisible
        ? InlineEditingSmallInput_scss_1.default.withoutRightButton
        : null,
    ]);
    return react_1.default.createElement(
      'div',
      {
        className: componentStyles,
        onBlur: this.onBlur,
        onMouseUp: () => {
          this.input.focus();
          this.onFocus();
        },
        role: 'presentation',
        'aria-hidden': true,
      },
      react_1.default.createElement(Input_1.Input, {
        className: inputStyles,
        themeOverrides: InlineEditingSmallInput_scss_1.default,
        type: 'text',
        maxLength: maxLength,
        label: inputFieldLabel,
        value: inputField.value,
        onChange: inputField.onChange,
        onFocus: inputField.onFocus,
        onBlur: inputField.onBlur,
        onKeyDown: (event) => this.handleInputKeyDown(event),
        error: isActive || inputBlocked ? inputField.error : null,
        disabled: isDisabled,
        placeholder: placeholder || '',
        ref: (input) => {
          this.inputField = input;
        },
        skin: InputSkin_1.InputSkin,
      }),
      !isDisabled &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          !isActive
            ? react_1.default.createElement(Button_1.Button, {
                className: InlineEditingSmallInput_scss_1.default.rightButton,
                label: react_1.default.createElement(
                  react_svg_inline_1.default,
                  {
                    svg: pen_inline_svg_1.default,
                    className: InlineEditingSmallInput_scss_1.default.penIcon,
                    // @ts-ignore ts-migrate(2322) FIXME: Type '{ svg: any; className: any; style: { pointer... Remove this comment to see the full error message
                    style: {
                      pointerEvents: 'none',
                    },
                  }
                ),
                onMouseUp: () => this.input.focus(),
                onMouseDown: (event) => {
                  event.persist();
                  event.preventDefault();
                  event.stopPropagation();
                },
                skin: ButtonSkin_1.ButtonSkin,
              })
            : react_1.default.createElement(
                react_1.default.Fragment,
                null,
                react_1.default.createElement(Button_1.Button, {
                  className: leftButtonStyles,
                  label: react_1.default.createElement(
                    react_svg_inline_1.default,
                    {
                      svg: close_cross_inline_svg_1.default,
                      className:
                        InlineEditingSmallInput_scss_1.default.crossIcon,
                      // @ts-ignore ts-migrate(2322) FIXME: Type '{ svg: any; className: any; style: { pointer... Remove this comment to see the full error message
                      style: {
                        pointerEvents: 'none',
                      },
                    }
                  ),
                  onMouseUp: () => {
                    this.onCancel();
                    this.input.blur();
                  },
                  onMouseDown: (event) => {
                    event.persist();
                    event.preventDefault();
                    event.stopPropagation();
                  },
                  skin: ButtonSkin_1.ButtonSkin,
                }),
                arrowIconIsVisible &&
                  react_1.default.createElement(Button_1.Button, {
                    className:
                      InlineEditingSmallInput_scss_1.default.rightButton,
                    label: react_1.default.createElement(
                      react_svg_inline_1.default,
                      {
                        svg: arrow_right_inline_svg_1.default,
                        className:
                          InlineEditingSmallInput_scss_1.default.arrowIcon,
                        // @ts-ignore ts-migrate(2322) FIXME: Type '{ svg: any; className: any; style: { pointer... Remove this comment to see the full error message
                        style: {
                          pointerEvents: 'none',
                        },
                      }
                    ),
                    onMouseUp: () => this.input.blur(),
                    onMouseDown: (event) => {
                      event.persist();
                      event.preventDefault();
                      event.stopPropagation();
                    },
                    skin: ButtonSkin_1.ButtonSkin,
                  })
              )
        )
    );
  }
};
InlineEditingSmallInput = __decorate(
  [mobx_react_1.observer],
  InlineEditingSmallInput
);
exports.default = InlineEditingSmallInput;
//# sourceMappingURL=InlineEditingSmallInput.js.map
