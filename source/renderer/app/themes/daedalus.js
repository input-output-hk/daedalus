// @flow
import SimpleAutocomplete from 'react-polymorph/lib/themes/simple/SimpleAutocomplete.scss';
import SimpleBubble from 'react-polymorph/lib/themes/simple/SimpleBubble.scss';
import SimpleButton from 'react-polymorph/lib/themes/simple/SimpleButton.scss';
import SimpleCheckbox from 'react-polymorph/lib/themes/simple/SimpleCheckbox.scss';
import SimpleDropdown from 'react-polymorph/lib/themes/simple/SimpleDropdown.scss';
import SimpleFormField from 'react-polymorph/lib/themes/simple/SimpleFormField.scss';
import SimpleInput from 'react-polymorph/lib/themes/simple/SimpleInput.scss';
import SimpleLoadingSpinner from 'react-polymorph/lib/themes/simple/SimpleLoadingSpinner.scss';
import SimpleModal from 'react-polymorph/lib/themes/simple/SimpleModal.scss';
import SimpleOptions from 'react-polymorph/lib/themes/simple/SimpleOptions.scss';
import SimpleRadio from 'react-polymorph/lib/themes/simple/SimpleRadio.scss';
import SimpleScrollBar from 'react-polymorph/lib/themes/simple/SimpleScrollBar.scss';
import SimpleSelect from 'react-polymorph/lib/themes/simple/SimpleSelect.scss';
import SimpleScrollBar from 'react-polymorph/lib/themes/simple/SimpleScrollBar.scss';
import SimpleStepper from 'react-polymorph/lib/themes/simple/SimpleStepper.scss';
import SimpleSwitch from 'react-polymorph/lib/themes/simple/SimpleSwitch.scss';
import SimpleTextArea from 'react-polymorph/lib/themes/simple/SimpleTextArea.scss';
import SimpleTooltip from 'react-polymorph/lib/themes/simple/SimpleTooltip.scss';
import SimpleLink from 'react-polymorph/lib/themes/simple/SimpleLink.scss';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';

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
  RADIO,
  SCROLLBAR,
  SELECT,
  STEPPER,
  SWITCH,
  TEXT_AREA,
  TOOLTIP,
} = IDENTIFIERS;

export const daedalusTheme = {
  [AUTOCOMPLETE]: SimpleAutocomplete,
  [BUBBLE]: SimpleBubble,
  [BUTTON]: SimpleButton,
  [CHECKBOX]: SimpleCheckbox,
  [DROPDOWN]: SimpleDropdown,
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
  [LINK]: SimpleLink,
  [LOADING_SPINNER]: SimpleLoadingSpinner,
  [MODAL]: SimpleModal,
  [OPTIONS]: SimpleOptions,
  [RADIO]: SimpleRadio,
  [SCROLLBAR]: SimpleScrollBar,
  [SELECT]: SimpleSelect,
  [STEPPER]: SimpleStepper,
  [SWITCH]: SimpleSwitch,
  [TEXT_AREA]: SimpleTextArea,
  [TOOLTIP]: SimpleTooltip,
};
