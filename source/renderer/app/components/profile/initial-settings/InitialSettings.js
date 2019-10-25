// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './InitialSettings.scss';
import ProfileSettingsForm from '../../widgets/forms/ProfileSettingsForm';
import type { ProfileSettingsFormProps } from '../../widgets/forms/ProfileSettingsForm';

@observer
export default class InitialSettings extends Component<ProfileSettingsFormProps> {
  static defaultProps = {
    error: null,
  };

  render() {
    const {
      onChangeItem,
      onSubmit,
      currentLocale,
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat,
      error,
    } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.centeredBox}>
          <ProfileSettingsForm
            onChangeItem={onChangeItem}
            onSubmit={onSubmit}
            currentLocale={currentLocale}
            currentNumberFormat={currentNumberFormat}
            currentDateFormat={currentDateFormat}
            currentTimeFormat={currentTimeFormat}
            error={error}
          />
        </div>
      </div>
    );
  }
}
