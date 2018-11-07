// @flow
import SimpleAutocomplete from 'react-polymorph/lib/themes/simple/SimpleAutocomplete.scss';
import SimpleBubble from 'react-polymorph/lib/themes/simple/SimpleBubble.scss';
import SimpleButton from 'react-polymorph/lib/themes/simple/SimpleButton.scss';
import SimpleCheckbox from 'react-polymorph/lib/themes/simple/SimpleCheckbox.scss';
import SimpleFormField from 'react-polymorph/lib/themes/simple/SimpleFormField.scss';
import SimpleInput from 'react-polymorph/lib/themes/simple/SimpleInput.scss';
import SimpleModal from 'react-polymorph/lib/themes/simple/SimpleModal.scss';
import SimpleOptions from 'react-polymorph/lib/themes/simple/SimpleOptions.scss';
import SimpleSelect from 'react-polymorph/lib/themes/simple/SimpleSelect.scss';
import SimpleSwitch from 'react-polymorph/lib/themes/simple/SimpleSwitch.scss';
import SimpleTextArea from 'react-polymorph/lib/themes/simple/SimpleTextArea.scss';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';

const {
  AUTOCOMPLETE,
  BUBBLE,
  BUTTON,
  CHECKBOX,
  FORM_FIELD,
  INPUT,
  MODAL,
  OPTIONS,
  SELECT,
  SWITCH,
  TEXT_AREA,
} = IDENTIFIERS;

export const daedalusTheme = {
  [AUTOCOMPLETE]: SimpleAutocomplete,
  [BUBBLE]: SimpleBubble,
  [BUTTON]: SimpleButton,
  [CHECKBOX]: SimpleCheckbox,
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
  [MODAL]: SimpleModal,
  [OPTIONS]: SimpleOptions,
  [SELECT]: SimpleSelect,
  [SWITCH]: SimpleSwitch,
  [TEXT_AREA]: SimpleTextArea,
};
