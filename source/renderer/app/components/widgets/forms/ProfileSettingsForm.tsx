import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Select } from 'react-polymorph/lib/components/Select';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { defineMessages, intlShape } from 'react-intl';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ProfileSettingsForm.scss' or... Remove this comment to see the full error message
import styles from './ProfileSettingsForm.scss';
import {
  LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  TIME_OPTIONS,
  PROFILE_SETTINGS,
} from '../../../config/profileConfig';

const messages = defineMessages({
  localeSelectLabel: {
    id: 'profile.settings.languageSelect.label',
    defaultMessage: '!!!Language',
    description: 'Label for the language select.',
  },
  numberFormatSelectLabel: {
    id: 'profile.settings.numberSelect.label',
    defaultMessage: '!!!Number format',
    description: 'Label for the number select.',
  },
  dateFormatSelectLabel: {
    id: 'profile.settings.dateSelect.label',
    defaultMessage: '!!!Date format',
    description: 'Label for the date select.',
  },
  timeFormatSelectLabel: {
    id: 'profile.settings.timeSelect.label',
    defaultMessage: '!!!Time format',
    description: 'Label for the time select.',
  },
  submitLabel: {
    id: 'profile.settings.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Language select" form submit button.',
  },
});
export type ProfileSettingsFormProps = {
  currentLocale: string;
  currentNumberFormat: string;
  currentDateFormat: string;
  currentTimeFormat: string;
  onChangeItem: (...args: Array<any>) => any;
  onSubmit?: (...args: Array<any>) => any;
  isSubmitting?: boolean;
  error?: LocalizableError | null | undefined;
};

@observer
class ProfileSettingsForm extends Component<ProfileSettingsFormProps> {
  static defaultProps = {
    onChangeItem: () => {},
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get locale() {
    const { props, context } = this;
    const options = LANGUAGE_OPTIONS.map((language) => ({
      value: language.value,
      label: context.intl.formatMessage(language.label),
    }));
    const value = props.currentLocale;
    return {
      value,
      options,
    };
  }

  get numberFormat() {
    return {
      options: NUMBER_OPTIONS,
      value: this.props.currentNumberFormat,
    };
  }

  get dateFormat() {
    const { currentLocale, currentDateFormat } = this.props;
    return {
      options:
        currentLocale === 'en-US'
          ? DATE_ENGLISH_OPTIONS
          : DATE_JAPANESE_OPTIONS,
      value: currentDateFormat,
    };
  }

  get timeFormat() {
    return {
      options: TIME_OPTIONS,
      value: this.props.currentTimeFormat,
    };
  }

  getSelect = (id: string) => {
    const { formatMessage } = this.context.intl;
    const { onChangeItem } = this.props;
    const { value, options } = (this as any)[id];
    return (
      <Select
        label={formatMessage(messages[`${id}SelectLabel`])}
        value={value}
        options={options}
        onChange={(v: string) => onChangeItem(id, v)}
        skin={SelectSkin}
        className={styles.select}
        key={id}
        optionHeight={50}
      />
    );
  };

  render() {
    const { error, onSubmit, isSubmitting } = this.props;
    const { formatMessage } = this.context.intl;
    const componentClassNames = classNames([styles.component, 'general']);
    return (
      <div className={componentClassNames}>
        {PROFILE_SETTINGS.map((param: string) => this.getSelect(param))}
        {error && <p className={styles.error}>{error}</p>}
        {onSubmit && (
          <Button
            className={classNames(['primary', styles.submitButton])}
            label={formatMessage(messages.submitLabel)}
            skin={ButtonSpinnerSkin}
            loading={isSubmitting}
            onClick={onSubmit}
          />
        )}
      </div>
    );
  }
}

export default ProfileSettingsForm;
