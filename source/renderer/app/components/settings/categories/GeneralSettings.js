// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ProfileSettingsForm from '../../widgets/forms/ProfileSettingsForm';
import type { ProfileSettingsFormProps } from '../../widgets/forms/ProfileSettingsForm';

@observer
export default class GeneralSettings extends Component<ProfileSettingsFormProps> {
  render() {
    const {
      onChangeItem,
      currentLocale,
      currentNumberFormat,
      currentDateEnglishFormat,
      currentDateJapaneseFormat,
      currentTimeFormat,
      error,
    } = this.props;
    return (
      <ProfileSettingsForm
        onChangeItem={onChangeItem}
        currentLocale={currentLocale}
        currentNumberFormat={currentNumberFormat}
        currentDateEnglishFormat={currentDateEnglishFormat}
        currentDateJapaneseFormat={currentDateJapaneseFormat}
        currentTimeFormat={currentTimeFormat}
        error={error}
      />
    );
  }
}
