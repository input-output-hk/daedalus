import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';

// react-polymorph components
import SimpleFormField from 'react-polymorph/lib/themes/simple/SimpleFormField.scss';
// import SimpleFormField from './simple/SimpleFormField.scss';
import SimpleInput from './simple/SimpleInput.scss';
import SimpleCheckbox from './simple/SimpleCheckbox.scss';
import SimpleSwitch from './simple/SimpleSwitch.scss';
import SimpleModal from './simple/SimpleModal.scss';
import SimpleButton from './simple/SimpleButton.scss';
import SimpleTextArea from './simple/SimpleTextArea.scss';
import SimpleAutocomplete from './simple/SimpleAutocomplete.scss';
import SimpleBubble from './simple/SimpleBubble.scss';
import SimpleOptions from './simple/SimpleOptions.scss';
import SimpleSelect from './simple/SimpleSelect.scss';

const {
  SELECT, INPUT, FORM_FIELD, CHECKBOX, SWITCH, MODAL,
  BUTTON, TEXT_AREA, AUTOCOMPLETE, OPTIONS, BUBBLE
} = IDENTIFIERS;

export const daedalusTheme = {
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
  [CHECKBOX]: SimpleCheckbox,
  [SWITCH]: SimpleSwitch,
  [MODAL]: SimpleModal,
  [BUTTON]: SimpleButton,
  [TEXT_AREA]: SimpleTextArea,
  [BUBBLE]: SimpleBubble,
  [OPTIONS]: SimpleOptions,
  [SELECT]: SimpleSelect,
  [AUTOCOMPLETE]: SimpleAutocomplete,
};
