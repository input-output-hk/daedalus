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

export const handleFormErrors = async (querySelector: string) => {
  try {
    const firstErrorLabel = await waitForExist(querySelector);
    firstErrorLabel.scrollIntoView({ behavior: 'smooth' });
    if (firstErrorLabel.nextSibling && firstErrorLabel.nextSibling.click) {
      setTimeout(() => firstErrorLabel.nextSibling.click(), 500);
    }
  } catch (err) {
    throw err;
  }
};
