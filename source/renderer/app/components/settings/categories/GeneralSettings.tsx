import React, { Component } from 'react';
import { observer } from 'mobx-react';
import ProfileSettingsForm from '../../widgets/forms/ProfileSettingsForm';
import type { ProfileSettingsFormProps } from '../../widgets/forms/ProfileSettingsForm';

@observer
class GeneralSettings extends Component<ProfileSettingsFormProps> {
  render() {
    const {
      onChangeItem,
      currentLocale,
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat,
      error,
    } = this.props;
    return (
      <ProfileSettingsForm
        onChangeItem={onChangeItem}
        currentLocale={currentLocale}
        currentNumberFormat={currentNumberFormat}
        currentDateFormat={currentDateFormat}
        currentTimeFormat={currentTimeFormat}
        error={error}
      />
    );
  }
}

export default GeneralSettings;
