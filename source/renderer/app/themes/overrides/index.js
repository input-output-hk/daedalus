import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import ButtonOverrides from './ButtonOverrides.scss';
import SwitchOverrides from './SwitchOverrides.scss';

const { BUTTON, SWITCH } = IDENTIFIERS;

export const themeOverrides = {
  [BUTTON]: ButtonOverrides,
  [SWITCH]: SwitchOverrides,
};
