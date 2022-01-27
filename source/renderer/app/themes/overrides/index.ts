import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './AutocompleteOverrides.scss' ... Remove this comment to see the full error message
import AutocompleteOverrides from './AutocompleteOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ButtonOverrides.scss' or its... Remove this comment to see the full error message
import ButtonOverrides from './ButtonOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CheckboxOverrides.scss' or i... Remove this comment to see the full error message
import CheckboxOverrides from './CheckboxOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './InputOverrides.scss' or its ... Remove this comment to see the full error message
import InputOverrides from './InputOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './LinkOverrides.scss' or its c... Remove this comment to see the full error message
import LinkOverrides from './LinkOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ModalOverrides.scss' or its ... Remove this comment to see the full error message
import ModalOverrides from './ModalOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './RadioOverrides.scss' or its ... Remove this comment to see the full error message
import RadioOverrides from './RadioOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ScrollbarOverrides.scss' or ... Remove this comment to see the full error message
import ScrollbarOverrides from './ScrollbarOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StepperOverrides.scss' or it... Remove this comment to see the full error message
import StepperOverrides from './StepperOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SwitchOverrides.scss' or its... Remove this comment to see the full error message
import SwitchOverrides from './SwitchOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SelectOverrides.scss' or its... Remove this comment to see the full error message
import SelectOverrides from './SelectOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './OptionsOverrides.scss' or it... Remove this comment to see the full error message
import OptionsOverrides from './OptionsOverrides.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PopOverOverrides.scss' or it... Remove this comment to see the full error message
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
