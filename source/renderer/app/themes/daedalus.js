import {
  SELECT, INPUT, FORM_FIELD, CHECKBOX, SWITCH, MODAL, BUTTON, TEXT_AREA, AUTOCOMPLETE, OPTIONS,
} from 'react-polymorph/lib/skins/simple/identifiers';

// react-polymorph components
import SimpleFormField from './simple/SimpleFormField.scss';
import SimpleSelect from './simple/SimpleSelect.scss';
import SimpleInput from './simple/SimpleInput.scss';
import SimpleCheckbox from './simple/SimpleCheckbox.scss';
import SimpleSwitch from './simple/SimpleSwitch.scss';
import SimpleModal from './simple/SimpleModal.scss';
import SimpleButton from './simple/SimpleButton.scss';
import SimpleTextArea from './simple/SimpleTextArea.scss';
import SimpleAutocomplete from './simple/SimpleAutocomplete.scss';
import SimpleOptions from './simple/SimpleOptions.scss';

export const daedalusTheme = {
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
  [SELECT]: SimpleSelect,
  [CHECKBOX]: SimpleCheckbox,
  [SWITCH]: SimpleSwitch,
  [MODAL]: SimpleModal,
  [BUTTON]: SimpleButton,
  [TEXT_AREA]: SimpleTextArea,
  [AUTOCOMPLETE]: SimpleAutocomplete,
  [OPTIONS]: SimpleOptions,
};
