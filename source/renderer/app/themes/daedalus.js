import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';

// react-polymorph components
import SimpleFormField from 'react-polymorph/lib/themes/simple/SimpleFormField.scss';
// import SimpleFormField from './simple/SimpleFormField.scss';
import SimpleInput from 'react-polymorph/lib/themes/simple/SimpleInput.scss';
// import SimpleInput from './simple/SimpleInput.scss';
import SimpleBubble from 'react-polymorph/lib/themes/simple/SimpleBubble.scss';
// import SimpleBubble from './simple/SimpleBubble.scss';
import SimpleButton from 'react-polymorph/lib/themes/simple/SimpleButton.scss';
// import SimpleButton from './simple/SimpleButton.scss';
import SimpleCheckbox from 'react-polymorph/lib/themes/simple/SimpleCheckbox.scss';
// import SimpleCheckbox from './simple/SimpleCheckbox.scss';
import SimpleTextArea from 'react-polymorph/lib/themes/simple/SimpleTextArea.scss';
// import SimpleTextArea from './simple/SimpleTextArea.scss';
import SimpleOptions from 'react-polymorph/lib/themes/simple/SimpleOptions.scss';
// import SimpleOptions from './simple/SimpleOptions.scss';
import SimpleSelect from 'react-polymorph/lib/themes/simple/SimpleSelect.scss';
// import SimpleSelect from './simple/SimpleSelect.scss';
import SimpleSwitch from 'react-polymorph/lib/themes/simple/SimpleSwitch.scss';
// import SimpleSwitch from './simple/SimpleSwitch.scss';
import SimpleModal from 'react-polymorph/lib/themes/simple/SimpleModal.scss';
// import SimpleModal from './simple/SimpleModal.scss';
import SimpleAutocomplete from 'react-polymorph/lib/themes/simple/SimpleAutocomplete.scss';
// import SimpleAutocomplete from './simple/SimpleAutocomplete.scss';

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
