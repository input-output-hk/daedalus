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

  onSubmit = async (values: { locale: string }) => {
    const { actions, stores } = this.props;
    const { isUpdateAvailable } = stores.nodeUpdate;
    const { updateLocale } = actions.profile;
    updateLocale.trigger(values);
    await rebuildApplicationMenu.send({ isUpdateAvailable });
  };

  handleSelectItem = async (param: string, values: string) => {
    if (param === 'language') {
      const { updateLocale } = this.props.actions.profile;
      updateLocale.trigger({ locale: values });
    }
  };

  render() {
    const { currentRoute } = this.props.stores.app;
    const {
      setProfileLocaleRequest,
      currentLocale,
      currentNumberFormat,
      currentDateEnglishFormat,
      currentDateJapaneseFormat,
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
          currentDateEnglishFormat={currentDateEnglishFormat}
          currentDateJapaneseFormat={currentDateJapaneseFormat}
          currentTimeFormat={currentTimeFormat}
          error={setProfileLocaleRequest.error}
        />
      </TopBarLayout>
    );
  }
}
