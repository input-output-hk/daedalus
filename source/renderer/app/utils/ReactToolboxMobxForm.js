// @flow
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
  focusElement?: ?boolean,
  asyncSelector?: ?boolean,
};

export const handleFormErrors = async (
  querySelector: string,
  options: HandleFormErrorsOptions = {}
) => {
  const { focusElement, asyncSelector } = options;

  const firstErrorLabel = asyncSelector
    ? await waitForExist(querySelector)
    : document.querySelector(querySelector);

  if (firstErrorLabel) {
    firstErrorLabel.scrollIntoView({ behavior: 'smooth' });
  }

  if (
    focusElement &&
    firstErrorLabel &&
    firstErrorLabel.parentNode instanceof HTMLElement
  ) {
    const input = firstErrorLabel.parentNode.querySelector('input');
    if (input) setTimeout(() => input.focus(), 500);
  }
};
