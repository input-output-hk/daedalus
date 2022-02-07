import MobxReactForm from 'mobx-react-form';
import { waitForExist } from './waitForExist';

export default class ReactToolboxMobxForm extends MobxReactForm {
  bindings() {
    return {
      ReactToolbox: {
        id: 'id',
        name: 'name',
        type: 'type',
        value: 'value',
        label: 'label',
        placeholder: 'hint',
        disabled: 'disabled',
        error: 'error',
        onChange: 'onChange',
        onFocus: 'onFocus',
        onBlur: 'onBlur',
      },
    };
  }
}
type HandleFormErrorsOptions = {
  focusElement?: boolean | null | undefined;
};
export const handleFormErrors = async (
  querySelector: string,
  options: HandleFormErrorsOptions = {}
) => {
  const { focusElement } = options;
  const firstErrorLabel = await waitForExist(querySelector);

  if (firstErrorLabel) {
    firstErrorLabel.scrollIntoView({
      behavior: 'smooth',
    });
  }

  if (
    focusElement &&
    firstErrorLabel &&
    firstErrorLabel.parentNode instanceof HTMLElement
  ) {
    const input = firstErrorLabel.parentNode.querySelector(
      'div:not(.SimpleAutocomplete_selectedWords) > input'
    );
    if (input) return setTimeout(() => input.focus(), 500);
  }

  return false;
};
