import {
  SELECT, INPUT, FORM_FIELD, CHECKBOX,
} from 'react-polymorph/lib/skins/simple/identifiers';
import overlay from 'react-toolbox/lib/overlay/theme.scss';
import navigation from 'react-toolbox/lib/navigation/theme.scss';
import input from './daedalus/input.scss';
import dialog from './daedalus/dialog.scss';
import button from './daedalus/button.scss';
import appBar from './daedalus/app-bar.scss';
import switchStyle from './daedalus/switch.scss';
import checkbox from './daedalus/checkbox.scss';

// react-polymorph components
import SimpleFormField from './simple/SimpleFormField.scss';
import SimpleSelect from './simple/SimpleSelect.scss';
import SimpleInput from './simple/SimpleInput.scss';
import SimpleCheckbox from './simple/SimpleCheckbox.scss';

export const daedalusTheme = {
  RTInput: input,
  RTOverlay: overlay,
  RTNavigation: navigation,
  RTDialog: dialog,
  RTButton: button,
  RTAppBar: appBar,
  RTSwitch: switchStyle,
  RTCheckbox: checkbox,
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
  [SELECT]: SimpleSelect,
  [CHECKBOX]: SimpleCheckbox,
};
