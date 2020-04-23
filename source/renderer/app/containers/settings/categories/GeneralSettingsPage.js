// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import GeneralSettings from '../../../components/settings/categories/GeneralSettings';
import { rebuildApplicationMenu } from '../../../ipc/rebuild-application-menu.js';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class GeneralSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleSelectItem = async (param: string, value: string) => {
    const { actions, stores } = this.props;
    const { isUpdateAvailable } = stores.nodeUpdate;
    const { updateUserLocalSetting } = actions.profile;
    updateUserLocalSetting.trigger({ param, value });
    if (param === 'locale') {
      await rebuildApplicationMenu.send({ isUpdateAvailable });
    }
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
