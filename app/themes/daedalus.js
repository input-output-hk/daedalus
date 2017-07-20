import {
  SELECT, INPUT, FORM_FIELD, CHECKBOX, SWITCH, MODAL, BUTTON,
} from 'react-polymorph/lib/skins/simple/identifiers';

// react-polymorph components
import SimpleFormField from './simple/SimpleFormField.scss';
import SimpleSelect from './simple/SimpleSelect.scss';
import SimpleInput from './simple/SimpleInput.scss';
import SimpleCheckbox from './simple/SimpleCheckbox.scss';
import SimpleSwitch from './simple/SimpleSwitch.scss';
import SimpleModal from './simple/SimpleModal.scss';
import SimpleButton from './simple/SimpleButton.scss';

export const daedalusTheme = {
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
  [SELECT]: SimpleSelect,
  [CHECKBOX]: SimpleCheckbox,
  [SWITCH]: SimpleSwitch,
  [MODAL]: SimpleModal,
  [BUTTON]: SimpleButton,
};
