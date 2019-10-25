// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import InitialSettings from '../../components/profile/initial-settings/InitialSettings';
import { rebuildApplicationMenu } from '../../ipc/rebuild-application-menu';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class InitialSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  onSubmit = async () => {
    const { actions } = this.props;
    const { finishInitialScreenSettings } = actions.profile;
    finishInitialScreenSettings.trigger();
  };

  handleSelectItem = async (param: string, value: string) => {
    const { actions, stores } = this.props;
    const { updateUserLocalSetting } = actions.profile;
    updateUserLocalSetting.trigger({ param, value });
    const { isUpdateAvailable } = stores.nodeUpdate;
    if (param === 'locale') {
      await rebuildApplicationMenu.send({ isUpdateAvailable });
    }
  };

  render() {
    const { currentRoute } = this.props.stores.app;
    const {
      setProfileLocaleRequest,
      currentLocale,
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat,
    } = this.props.stores.profile;
    const isSubmitting = setProfileLocaleRequest.isExecuting;
    const topbar = (
      <TopBar currentRoute={currentRoute} showSubMenuToggle={false} />
    );
    return (
      <TopBarLayout topbar={topbar}>
        <InitialSettings
          onChangeItem={this.handleSelectItem}
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          currentLocale={currentLocale}
          currentNumberFormat={currentNumberFormat}
          currentDateFormat={currentDateFormat}
          currentTimeFormat={currentTimeFormat}
          error={setProfileLocaleRequest.error}
        />
      </TopBarLayout>
    );
  }
}
