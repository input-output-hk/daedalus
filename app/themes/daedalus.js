import {
  cyan500,
  pinkA200,
  grey100, grey300, grey400, grey500,
  white, darkBlack, fullBlack,
} from 'material-ui/styles/colors';
import { fade } from 'material-ui/utils/colorManipulator';
import spacing from 'material-ui/styles/spacing';
import overlay from 'react-toolbox/lib/overlay/theme.scss';
import navigation from 'react-toolbox/lib/navigation/theme.scss';
import dropdown from './daedalus/dropdown.scss';
import input from './daedalus/input.scss';
import dialog from './daedalus/dialog.scss';
import button from './daedalus/button.scss';

export const materialUiTheme = {
  spacing,
  fontFamily: 'SFUIDisplay',
  palette: {
    primary1Color: '#345078',
    primary2Color: '#345078',
    primary3Color: grey400,
    accent1Color: pinkA200,
    accent2Color: grey100,
    accent3Color: grey500,
    textColor: darkBlack,
    secondaryTextColor: fade(darkBlack, 0.54),
    alternateTextColor: white,
    canvasColor: white,
    borderColor: grey300,
    disabledColor: fade(darkBlack, 0.3),
    pickerHeaderColor: cyan500,
    clockCircleColor: fade(darkBlack, 0.07),
    shadowColor: fullBlack,
  },
  appBar: {
    color: '#243e62'
  }
};

export const daedalusTheme = {
  RTInput: input,
  RTDropdown: dropdown,
  RTOverlay: overlay,
  RTNavigation: navigation,
  RTDialog: dialog,
  RTButton: button,
};
