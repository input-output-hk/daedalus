import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import ButtonOverrides from './ButtonOverrides.scss';
import ModalOverrides from './ModalOverrides.scss';
import SwitchOverrides from './SwitchOverrides.scss';

const { BUTTON, MODAL, SWITCH } = IDENTIFIERS;

export const themeOverrides = {
  [BUTTON]: ButtonOverrides,
  [MODAL]: ModalOverrides,
  [SWITCH]: SwitchOverrides,
};
