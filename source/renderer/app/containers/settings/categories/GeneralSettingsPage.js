// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import GeneralSettings from '../../../components/settings/categories/GeneralSettings';
import { rebuildApplicationMenu } from '../../../ipc/rebuild-application-menu';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class GeneralSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleSelectItem = async (param: string, value: string) => {
    const { actions } = this.props;
    const { updateUserLocalSetting } = actions.profile;
    updateUserLocalSetting.trigger({ param, value });
  };

  render() {
    const {
      setProfileLocaleRequest,
      currentLocale,
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat,
    } = this.props.stores.profile;
    return (
      <GeneralSettings
        onChangeItem={this.handleSelectItem}
        currentLocale={currentLocale}
        currentNumberFormat={currentNumberFormat}
        currentDateFormat={currentDateFormat}
        currentTimeFormat={currentTimeFormat}
        error={setProfileLocaleRequest.error}
      />
    );
  }
}
