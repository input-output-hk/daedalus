import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import AutocompleteOverrides from './AutocompleteOverrides.scss';
import ButtonOverrides from './ButtonOverrides.scss';
import ModalOverrides from './ModalOverrides.scss';
import SwitchOverrides from './SwitchOverrides.scss';
import SelectOverrides from './SelectOverrides.scss';

const { AUTOCOMPLETE, BUTTON, MODAL, SWITCH, SELECT } = IDENTIFIERS;

export const themeOverrides = {
  [AUTOCOMPLETE]: AutocompleteOverrides,
  [BUTTON]: ButtonOverrides,
  [MODAL]: ModalOverrides,
  [SWITCH]: SwitchOverrides,
  [SELECT]: SelectOverrides,
};
