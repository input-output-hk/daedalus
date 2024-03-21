'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.themeOverrides = void 0;
const API_1 = require('@react-polymorph/themes/API');
const AutocompleteOverrides_scss_1 = __importDefault(
  require('./AutocompleteOverrides.scss')
);
const ButtonOverrides_scss_1 = __importDefault(
  require('./ButtonOverrides.scss')
);
const CheckboxOverrides_scss_1 = __importDefault(
  require('./CheckboxOverrides.scss')
);
const InputOverrides_scss_1 = __importDefault(require('./InputOverrides.scss'));
const LinkOverrides_scss_1 = __importDefault(require('./LinkOverrides.scss'));
const ModalOverrides_scss_1 = __importDefault(require('./ModalOverrides.scss'));
const RadioOverrides_scss_1 = __importDefault(require('./RadioOverrides.scss'));
const ScrollbarOverrides_scss_1 = __importDefault(
  require('./ScrollbarOverrides.scss')
);
const StepperOverrides_scss_1 = __importDefault(
  require('./StepperOverrides.scss')
);
const SwitchOverrides_scss_1 = __importDefault(
  require('./SwitchOverrides.scss')
);
const SelectOverrides_scss_1 = __importDefault(
  require('./SelectOverrides.scss')
);
const OptionsOverrides_scss_1 = __importDefault(
  require('./OptionsOverrides.scss')
);
const PopOverOverrides_scss_1 = __importDefault(
  require('./PopOverOverrides.scss')
);
const {
  AUTOCOMPLETE,
  BUTTON,
  CHECKBOX,
  INPUT,
  LINK,
  MODAL,
  SWITCH,
  SELECT,
  OPTIONS,
  POP_OVER,
  RADIO,
  SCROLLBAR,
  STEPPER,
} = API_1.IDENTIFIERS;
exports.themeOverrides = {
  [AUTOCOMPLETE]: AutocompleteOverrides_scss_1.default,
  [BUTTON]: ButtonOverrides_scss_1.default,
  [CHECKBOX]: CheckboxOverrides_scss_1.default,
  [INPUT]: InputOverrides_scss_1.default,
  [LINK]: LinkOverrides_scss_1.default,
  [MODAL]: ModalOverrides_scss_1.default,
  [RADIO]: RadioOverrides_scss_1.default,
  [SCROLLBAR]: ScrollbarOverrides_scss_1.default,
  [STEPPER]: StepperOverrides_scss_1.default,
  [SWITCH]: SwitchOverrides_scss_1.default,
  [SELECT]: SelectOverrides_scss_1.default,
  [OPTIONS]: OptionsOverrides_scss_1.default,
  [POP_OVER]: PopOverOverrides_scss_1.default,
};
//# sourceMappingURL=index.js.map
