// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Select from 'react-polymorph/lib/components/Select';
import SelectSkin from 'react-polymorph/lib/skins/simple/SelectSkin';
import Button from 'react-toolbox/lib/button/Button';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './LanguageSelectionForm.scss';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const messages = defineMessages({
  languageSelectLabel: {
    id: 'profile.languageSelect.form.languageSelectLabel',
    defaultMessage: '!!!Select your language',
    description: 'Label for the language select.'
  },
  submitLabel: {
    id: 'profile.languageSelect.form.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Language select" form submit button.'
  },
});

@observer
export default class LanguageSelectionForm extends Component {

  props: {
    languages: Array<{ value: string, label: ReactIntlMessage }>,
    onSubmit: Function,
    isSubmitting: boolean,
    error?: ?LocalizableError,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { languageId } = form.values();
        this.props.onSubmit({ locale: languageId });
      },
      onError: () => {}
    });
  };

  form = new ReactToolboxMobxForm({
    fields: {
      languageId: {
        label: this.context.intl.formatMessage(messages.languageSelectLabel),
        value: this.props.languages[0].value,
        bindings: 'ReactToolbox',
      }
    }
  }, {
    options: {
      validateOnChange: false,
    },
  });

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { languages, isSubmitting, error } = this.props;
    const languageId = form.$('languageId');
    const languageOptions = languages.map(language => ({
      value: language.value,
      label: intl.formatMessage(language.label)
    }));

    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>

          <Select
            className={styles.languageSelect}
            options={languageOptions}
            {...languageId.bind()}
            skin={<SelectSkin />}
          />

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <Button
            className={isSubmitting ? styles.submitButtonSpinning : styles.submitButton}
            label={intl.formatMessage(messages.submitLabel)}
            onMouseUp={this.submit}
            primary
          />

        </div>
      </div>
    );
  }

}
