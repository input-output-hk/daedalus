import overlay from 'react-toolbox/lib/overlay/theme.scss';
import navigation from 'react-toolbox/lib/navigation/theme.scss';
import dropdown from './daedalus/dropdown.scss';
import input from './daedalus/input.scss';
import dialog from './daedalus/dialog.scss';
import button from './daedalus/button.scss';
import appBar from './daedalus/app-bar.scss';
import switchStyle from './daedalus/switch.scss';
import checkbox from './daedalus/checkbox.scss';
import { INPUT, FORM_FIELD } from 'react-polymorph/lib/skins/simple/identifiers';
import SimpleFormField from 'react-polymorph/lib/themes/simple/SimpleFormField.scss';
import SimpleInput from 'react-polymorph/lib/themes/simple/SimpleInput.scss';

export const daedalusTheme = {
  RTInput: input,
  RTDropdown: dropdown,
  RTOverlay: overlay,
  RTNavigation: navigation,
  RTDialog: dialog,
  RTButton: button,
  RTAppBar: appBar,
  RTSwitch: switchStyle,
  RTCheckbox: checkbox,
  [FORM_FIELD]: SimpleFormField,
  [INPUT]: SimpleInput,
};
