import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import AutocompleteOverrides from './AutocompleteOverrides.scss';
import ButtonOverrides from './ButtonOverrides.scss';
import CheckboxOverrides from './CheckboxOverrides.scss';
import InputOverrides from './InputOverrides.scss';
import LinkOverrides from './LinkOverrides.scss';
import ModalOverrides from './ModalOverrides.scss';
import RadioOverrides from './RadioOverrides.scss';
import ScrollbarOverrides from './ScrollbarOverrides.scss';
import StepperOverrides from './StepperOverrides.scss';
import SwitchOverrides from './SwitchOverrides.scss';
import SelectOverrides from './SelectOverrides.scss';
import OptionsOverrides from './OptionsOverrides.scss';
import PopOverOverrides from './PopOverOverrides.scss';

const {
  AUTOCOMPLETE,
  BUTTON,
  CHECKBOX,
  INPUT,
  LINK,
  MODAL,
  SWITCH,
  SELECT,
  OPTIONS,
  POP_OVER,
  RADIO,
  SCROLLBAR,
  STEPPER,
} = IDENTIFIERS;

export const themeOverrides = {
  [AUTOCOMPLETE]: AutocompleteOverrides,
  [BUTTON]: ButtonOverrides,
  [CHECKBOX]: CheckboxOverrides,
  [INPUT]: InputOverrides,
  [LINK]: LinkOverrides,
  [MODAL]: ModalOverrides,
  [RADIO]: RadioOverrides,
  [SCROLLBAR]: ScrollbarOverrides,
  [STEPPER]: StepperOverrides,
  [SWITCH]: SwitchOverrides,
  [SELECT]: SelectOverrides,
  [OPTIONS]: OptionsOverrides,
  [POP_OVER]: PopOverOverrides,
};
