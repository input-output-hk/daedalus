// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Select } from 'react-polymorph/lib/components/Select';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './ProfileSettingsForm.scss';
import {
  LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  TIME_OPTIONS,
} from '../../../config/profileConfig';

const messages = defineMessages({
  languageSelectLabel: {
    id: 'profile.settings.languageSelect.label',
    defaultMessage: '!!!Language',
    description: 'Label for the language select.',
  },
  numberSelectLabel: {
    id: 'profile.settings.numberSelect.label',
    defaultMessage: '!!!Number format',
    description: 'Label for the number select.',
  },
  dateSelectLabel: {
    id: 'profile.settings.dateSelect.label',
    defaultMessage: '!!!Date format',
    description: 'Label for the date select.',
  },
  timeSelectLabel: {
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
  currentLocale: string,
  currentNumberFormat: string,
  currentDateEnglishFormat: string,
  currentDateJapaneseFormat: string,
  currentTimeFormat: string,
  onChangeItem: Function,
  onSubmit?: Function,
  isSubmitting?: boolean,
  error?: ?LocalizableError,
};

@observer
export default class ProfileSettingsForm extends Component<ProfileSettingsFormProps> {
  static defaultProps = {
    onChangeItem: () => {},
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { languageId, numberId, dateId, timeId } = form.values();
        const { onSubmit } = this.props;
        if (onSubmit) {
          onSubmit({
            locale: languageId,
            numberFormat: numberId,
            dateFormat: dateId,
            timeFormat: timeId,
          });
        }
      },
      onError: () => {},
    });
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        languageId: {
          label: this.context.intl.formatMessage(messages.languageSelectLabel),
          value: this.props.currentLocale,
        },
        numberId: {
          label: this.context.intl.formatMessage(messages.numberSelectLabel),
          value: this.props.currentNumberFormat,
        },
        dateId: {
          label: this.context.intl.formatMessage(messages.dateSelectLabel),
          value:
            this.props.currentLocale === 'en-US'
              ? this.props.currentDateEnglishFormat
              : this.props.currentDateJapaneseFormat,
        },
        timeId: {
          label: this.context.intl.formatMessage(messages.timeSelectLabel),
          value: this.props.currentTimeFormat,
        },
      },
    },
    {
      options: {
        validateOnChange: false,
      },
    }
  );

  render() {
    const {
      error,
      onChangeItem,
      onSubmit,
      isSubmitting,
      currentLocale,
      currentNumberFormat,
      currentDateEnglishFormat,
      currentDateJapaneseFormat,
      currentTimeFormat,
    } = this.props;
    const { intl } = this.context;
    const { form } = this;
    const languageId = form.$('languageId');
    const numberId = form.$('numberId');
    const dateId = form.$('dateId');
    const timeId = form.$('timeId');
    const languageOptions = LANGUAGE_OPTIONS.map(language => ({
      value: language.value,
      label: intl.formatMessage(language.label),
    }));
    const numberOptions = NUMBER_OPTIONS;
    const dateEnglishOptions = DATE_ENGLISH_OPTIONS;
    const dateJapaneseOptions = DATE_JAPANESE_OPTIONS;
    const dateOptions =
      currentLocale === 'en-US' ? dateEnglishOptions : dateJapaneseOptions;
    const timeOptions = TIME_OPTIONS;
    const currentDateFormat =
      currentLocale === 'en-US'
        ? currentDateEnglishFormat
        : currentDateJapaneseFormat;
    const componentClassNames = classNames([styles.component, 'general']);
    return (
      <div className={componentClassNames}>
        <Select
          className={styles.select}
          options={languageOptions}
          {...languageId.bind()}
          onChange={(value: string) => onChangeItem('locale', value)}
          skin={SelectSkin}
        />
        <Select
          className={styles.select}
          options={numberOptions}
          {...numberId.bind()}
          value={currentNumberFormat}
          onChange={(value: string) => onChangeItem('numberFormat', value)}
          skin={SelectSkin}
        />
        <Select
          className={styles.select}
          options={dateOptions}
          {...dateId.bind()}
          value={currentDateFormat}
          onChange={(value: string) => onChangeItem('dateFormat', value)}
          skin={SelectSkin}
        />
        <Select
          className={styles.select}
          options={timeOptions}
          {...timeId.bind()}
          value={currentTimeFormat}
          onChange={(value: string) => onChangeItem('timeFormat', value)}
          skin={SelectSkin}
        />

        {error && <p className={styles.error}>{error}</p>}

        {onSubmit && (
          <Button
            className={classNames(['primary', styles.submitButton])}
            label={intl.formatMessage(messages.submitLabel)}
            skin={ButtonSpinnerSkin}
            loading={isSubmitting}
            onClick={this.submit}
          />
        )}
      </div>
    );
  }
}
