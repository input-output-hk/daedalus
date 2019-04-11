// @flow
import MobxReactForm from 'mobx-react-form';

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

export const handleFormErrors = (form: ReactToolboxMobxForm) => {
  try {
    const [firstErrorInputName] = Object.entries(form.errors()).filter(
      ([x, err]) => x && err !== null
    )[0];
    const [firstErrorInputDomElement] = document.getElementsByName(
      firstErrorInputName
    );
    if (!firstErrorInputDomElement) return false;
    return firstErrorInputDomElement.scrollIntoView({ behavior: 'smooth' });
  } catch (e) {
    throw e;
  }
};
