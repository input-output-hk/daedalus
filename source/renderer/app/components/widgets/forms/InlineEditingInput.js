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
/* eslint-disable react/no-did-update-set-state */
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const Button_1 = require('@react-polymorph/components/Button');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Input_1 = require('@react-polymorph/components/Input');
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const InlineEditingInput_scss_1 = __importDefault(
  require('./InlineEditingInput.scss')
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
const spinner_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/spinner-ic.inline.svg')
);
const numbersConfig_1 = require('../../../config/numbersConfig');
const messages = (0, react_intl_1.defineMessages)({
  change: {
    id: 'inline.editing.input.change.label',
    defaultMessage: '!!!change',
    description: 'Label "change" on inline editing inputs in inactive state.',
  },
  cancel: {
    id: 'inline.editing.input.cancel.label',
    defaultMessage: '!!!cancel',
    description: 'Label "cancel" on inline editing inputs in inactive state.',
  },
  changesSaved: {
    id: 'inline.editing.input.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description:
      'Message "Your changes have been saved" for inline editing (eg. on Profile Settings page).',
  },
});
let InlineEditingInput = class InlineEditingInput extends react_1.Component {
  static defaultProps = {
    validateOnChange: true,
    valueErrorMessage: '',
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    isActive: false,
    hasChanged: false,
    successfullyUpdated: false,
  };
  validator = new ReactToolboxMobxForm_1.default(
    {
      fields: {
        inputField: {
          value: this.props.value,
          validators: [
            ({ field }) => {
              const { value } = field;
              const { valueErrorMessage } = this.props;
              const errorMessage =
                typeof valueErrorMessage === 'function'
                  ? valueErrorMessage(value)
                  : valueErrorMessage;
              return [this.props.isValid(value), errorMessage || null];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: this.props.validateOnChange,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    this.validator.submit({
      onSuccess: async (form) => {
        this.setInputBlur();
        const { inputField } = form.values();
        const { onSubmit, errorMessage } = this.props;
        if (!inputField) {
          return;
        }
        if (inputField !== this.props.value || errorMessage) {
          this.setState({
            hasChanged: true,
            successfullyUpdated: false,
          });
          await onSubmit(inputField);
          this.setState({
            hasChanged: false,
            successfullyUpdated: true,
          });
        } else {
          this.setState({
            hasChanged: false,
          });
        }
      },
    });
  };
  handleInputKeyDown = (event) => {
    if (event.which === numbersConfig_1.ENTER_KEY_CODE) {
      this.submit();
    } else if (event.which === numbersConfig_1.ESCAPE_KEY_CODE) {
      this.onCancel();
    }
  };
  onFocus = () => {
    const { disabled, onFocus, readOnly } = this.props;
    if (!disabled && !readOnly) {
      this.setState({
        isActive: true,
      });
      if (onFocus) onFocus();
    }
  };
  onBlur = (event) => {
    event.stopPropagation();
    event.preventDefault();
    const { disabled, readOnly, onBlur } = this.props;
    this.setState({
      isActive: false,
    });
    if (!disabled && !readOnly && onBlur) {
      onBlur();
    }
  };
  onCancel = () => {
    const { value, onCancel, errorMessage } = this.props;
    const inputField = this.validator.$('inputField');
    const newValue = !errorMessage ? value : '';
    inputField.set(newValue);
    if (onCancel) onCancel();
    this.setInputFocus();
    this.setState({
      hasChanged: true,
    });
  };
  setInputFocus = () => {
    const input = this.inputElement;
    if (input instanceof HTMLElement) input.focus();
  };
  setInputBlur = () => {
    const input = this.inputElement;
    if (input instanceof HTMLElement) input.blur();
  };
  // @ts-ignore ts-migrate(2370) FIXME: A rest parameter must be of an array type.
  onChange = (...props) => {
    this.setState({
      hasChanged: true,
    });
    const inputField = this.validator.$('inputField');
    // @ts-ignore ts-migrate(2488) FIXME: Type 'KeyboardEvent' must have a '[Symbol.iterator... Remove this comment to see the full error message
    inputField.onChange(...props);
  };
  componentDidUpdate({ value: prevValue, errorMessage: prevError }) {
    const { value: nextValue, errorMessage: nextError } = this.props;
    const inputField = this.validator.$('inputField');
    // If there's an error, we focus the input again
    if (nextError) {
      this.setInputFocus();
    } else if (prevError && !nextError) {
      // else we blur it
      this.setInputBlur();
    }
    // In case the `value` prop was updated
    // we need to manually update the ReactToolboxMobxForm input field
    if (prevValue !== nextValue) {
      inputField.set(nextValue);
      if (nextValue === '') {
        this.setState({
          hasChanged: false,
        });
      }
    }
    // If the `value` props was updated
    // after a submit action
    // we show the `success` message
    const successfullyUpdated = !!nextValue && prevValue !== nextValue;
    if (successfullyUpdated) {
      this.setState({
        successfullyUpdated,
      });
    }
  }
  inputElement;
  preventDefaultHelper = (event) => {
    event.preventDefault();
    event.stopPropagation();
  };
  render() {
    const { validator } = this;
    const {
      className,
      label,
      maxLength,
      placeholder,
      disabled,
      readOnly,
      isLoading,
      errorMessage,
    } = this.props;
    const { isActive, hasChanged } = this.state;
    let { successfullyUpdated } = this.props;
    if (successfullyUpdated === undefined) {
      ({ successfullyUpdated } = this.state);
    }
    const { intl } = this.context;
    const inputField = validator.$('inputField');
    let error;
    if (inputField.error) error = inputField.error;
    else if (!hasChanged) error = !!errorMessage;
    const showEditButton =
      !isActive && !isLoading && !hasChanged && label.length && !readOnly;
    const showFocusButtons =
      !isLoading && !disabled && !readOnly && (isActive || hasChanged);
    const showLoadingButton = isLoading;
    const componentStyles = (0, classnames_1.default)([
      className,
      InlineEditingInput_scss_1.default.component,
      isActive ? null : InlineEditingInput_scss_1.default.inactive,
      readOnly ? InlineEditingInput_scss_1.default.readOnly : null,
      showEditButton || showLoadingButton
        ? InlineEditingInput_scss_1.default.twoButtons
        : null,
      showFocusButtons ? InlineEditingInput_scss_1.default.twoButtons : null,
    ]);
    const inputStyles = (0, classnames_1.default)([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer',
    ]);
    const buttonsWrapperStyles = (0, classnames_1.default)([
      InlineEditingInput_scss_1.default.buttonsWrapper,
      readOnly ? InlineEditingInput_scss_1.default.readOnly : null,
    ]);
    const editButtonStyles = (0, classnames_1.default)([
      InlineEditingInput_scss_1.default.button,
      InlineEditingInput_scss_1.default.editButton,
    ]);
    const cancelButtonStyles = (0, classnames_1.default)([
      InlineEditingInput_scss_1.default.button,
      InlineEditingInput_scss_1.default.cancelButton,
    ]);
    const okButtonStyles = (0, classnames_1.default)([
      InlineEditingInput_scss_1.default.button,
      InlineEditingInput_scss_1.default.okButton,
    ]);
    const submittingButtonStyles = (0, classnames_1.default)([
      InlineEditingInput_scss_1.default.button,
      InlineEditingInput_scss_1.default.submittingButton,
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentStyles },
      react_1.default.createElement(Input_1.Input, {
        ...inputField.bind(),
        className: inputStyles,
        placeholder: placeholder || '',
        themeOverrides: InlineEditingInput_scss_1.default,
        type: 'text',
        maxLength: maxLength,
        label: label,
        onFocus: this.onFocus,
        onBlur: this.onBlur,
        onChange: this.onChange,
        onKeyDown: (event) => this.handleInputKeyDown(event),
        error: isActive ? error : !!error,
        disabled: disabled,
        readOnly: readOnly,
        ref: (input) => {
          if (!this.inputElement) {
            this.inputElement = (0, lodash_1.get)(
              input,
              'inputElement.current'
            );
          }
        },
      }),
      react_1.default.createElement(
        'div',
        {
          className: buttonsWrapperStyles,
          // @ts-ignore ts-migrate(2322) FIXME: Type '(event: KeyboardEvent) => void' is not assig... Remove this comment to see the full error message
          onMouseDown: this.preventDefaultHelper,
          // @ts-ignore ts-migrate(2322) FIXME: Type '(event: KeyboardEvent) => void' is not assig... Remove this comment to see the full error message
          onMouseUp: this.preventDefaultHelper,
        },
        showEditButton &&
          react_1.default.createElement(Button_1.Button, {
            className: editButtonStyles,
            onMouseUp: this.setInputFocus,
            label: react_1.default.createElement(react_svg_inline_1.default, {
              svg: pen_inline_svg_1.default,
              className: InlineEditingInput_scss_1.default.icon,
            }),
          }),
        showFocusButtons &&
          react_1.default.createElement(Button_1.Button, {
            className: cancelButtonStyles,
            onClick: this.onCancel,
            label: react_1.default.createElement(react_svg_inline_1.default, {
              svg: close_cross_inline_svg_1.default,
              className: InlineEditingInput_scss_1.default.icon,
            }),
          }),
        showFocusButtons &&
          react_1.default.createElement(Button_1.Button, {
            className: okButtonStyles,
            onMouseUp: this.submit,
            label: react_1.default.createElement(react_svg_inline_1.default, {
              svg: arrow_right_inline_svg_1.default,
              className: InlineEditingInput_scss_1.default.icon,
            }),
          }),
        showLoadingButton &&
          react_1.default.createElement(Button_1.Button, {
            className: submittingButtonStyles,
            onMouseUp: () => {},
            label: react_1.default.createElement(react_svg_inline_1.default, {
              svg: spinner_ic_inline_svg_1.default,
              className: InlineEditingInput_scss_1.default.icon,
            }),
          })
      ),
      successfullyUpdated &&
        react_1.default.createElement(
          'div',
          { className: InlineEditingInput_scss_1.default.savingResultLabel },
          intl.formatMessage(messages.changesSaved)
        ),
      errorMessage &&
        !hasChanged &&
        react_1.default.createElement(
          'div',
          { className: InlineEditingInput_scss_1.default.errorMessage },
          errorMessage
        )
    );
  }
};
InlineEditingInput = __decorate([mobx_react_1.observer], InlineEditingInput);
exports.default = InlineEditingInput;
//# sourceMappingURL=InlineEditingInput.js.map
