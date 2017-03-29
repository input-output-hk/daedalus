// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import classNames from 'classnames';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './GeneralSettings.scss';

const messages = defineMessages({
  languageSelectLabel: {
    id: 'settings.general.languageSelect.label',
    defaultMessage: '!!!Language',
    description: 'Label for the language select.'
  },
});

@observer
export default class GeneralSettings extends Component {

  static propTypes = {
    languages: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.string.isRequired,
      label: PropTypes.object.isRequired,
    })).isRequired,
    currentLocale: PropTypes.string,
    onSelectLanguage: PropTypes.func.isRequired,
    isSubmitting: PropTypes.bool.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  selectLanguage = (values: { locale: string }) => {
    this.props.onSelectLanguage({ locale: values });
  };

  form = new ReactToolboxMobxForm({
    fields: {
      languageId: {
        label: this.context.intl.formatMessage(messages.languageSelectLabel),
        value: this.props.currentLocale,
        bindings: 'ReactToolbox',
      }
    }
  }, {
    options: {
      validateOnChange: false,
    }
  });

  render() {
    const { languages, isSubmitting, error } = this.props;
    const { intl } = this.context;
    const { form } = this;
    const languageId = form.$('languageId');
    const languageOptions = languages.map(language => ({
      value: language.value,
      label: intl.formatMessage(language.label)
    }));
    const componentClassNames = classNames([styles.component, 'general']);
    const languageSelectClassNames = classNames([
      styles.language,
      isSubmitting ? styles.submitLanguageSpinner : null,
    ]);
    return (
      <div className={componentClassNames}>

        <Dropdown
          className={languageSelectClassNames}
          source={languageOptions}
          {...languageId.bind()}
          onChange={this.selectLanguage}
        />

        {error && <p className={styles.error}>{error}</p>}

      </div>
    );
  }

}
