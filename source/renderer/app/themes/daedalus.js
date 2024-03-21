'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.daedalusTheme = void 0;
const SimpleAutocomplete_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleAutocomplete.scss')
);
const SimpleBubble_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleBubble.scss')
);
const SimpleButton_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleButton.scss')
);
const SimpleCheckbox_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleCheckbox.scss')
);
const SimpleDropdown_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleDropdown.scss')
);
const SimpleFormField_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleFormField.scss')
);
const SimpleInput_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleInput.scss')
);
const SimpleLoadingSpinner_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleLoadingSpinner.scss')
);
const SimpleModal_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleModal.scss')
);
const SimpleOptions_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleOptions.scss')
);
const SimplePasswordInput_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimplePasswordInput.scss')
);
const SimplePopOver_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimplePopOver.scss')
);
const SimpleRadio_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleRadio.scss')
);
const SimpleScrollBar_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleScrollBar.scss')
);
const SimpleSelect_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleSelect.scss')
);
const SimpleStepper_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleStepper.scss')
);
const SimpleSwitch_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleSwitch.scss')
);
const SimpleTextArea_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleTextArea.scss')
);
const SimpleTooltip_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleTooltip.scss')
);
const SimpleLink_scss_1 = __importDefault(
  require('@react-polymorph/themes/simple/SimpleLink.scss')
);
const API_1 = require('@react-polymorph/themes/API');
const {
  AUTOCOMPLETE,
  BUBBLE,
  BUTTON,
  CHECKBOX,
  DROPDOWN,
  FORM_FIELD,
  INPUT,
  LINK,
  LOADING_SPINNER,
  MODAL,
  OPTIONS,
  PASSWORD_INPUT,
  POP_OVER,
  RADIO,
  SCROLLBAR,
  SELECT,
  STEPPER,
  SWITCH,
  TEXT_AREA,
  TOOLTIP,
} = API_1.IDENTIFIERS;
exports.daedalusTheme = {
  [AUTOCOMPLETE]: SimpleAutocomplete_scss_1.default,
  [BUBBLE]: SimpleBubble_scss_1.default,
  [BUTTON]: SimpleButton_scss_1.default,
  [CHECKBOX]: SimpleCheckbox_scss_1.default,
  [DROPDOWN]: SimpleDropdown_scss_1.default,
  [FORM_FIELD]: SimpleFormField_scss_1.default,
  [INPUT]: SimpleInput_scss_1.default,
  [LINK]: SimpleLink_scss_1.default,
  [LOADING_SPINNER]: SimpleLoadingSpinner_scss_1.default,
  [MODAL]: SimpleModal_scss_1.default,
  [OPTIONS]: SimpleOptions_scss_1.default,
  [PASSWORD_INPUT]: SimplePasswordInput_scss_1.default,
  [POP_OVER]: SimplePopOver_scss_1.default,
  [RADIO]: SimpleRadio_scss_1.default,
  [SCROLLBAR]: SimpleScrollBar_scss_1.default,
  [SELECT]: SimpleSelect_scss_1.default,
  [STEPPER]: SimpleStepper_scss_1.default,
  [SWITCH]: SimpleSwitch_scss_1.default,
  [TEXT_AREA]: SimpleTextArea_scss_1.default,
  [TOOLTIP]: SimpleTooltip_scss_1.default,
};
//# sourceMappingURL=daedalus.js.map
