// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import GeneralSettings from '../../../components/settings/categories/GeneralSettings';
import { REBUILD_APPLICATION_MENU } from '../../../../../common/ipc/api';
import type { InjectedProps } from '../../../types/injectedPropsType';

const { ipcRenderer } = global;

@inject('stores', 'actions') @observer
export default class GeneralSettingsPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onSelectLanguage = (values: { locale: string }) => {
    this.props.actions.profile.updateLocale.trigger(values);
    ipcRenderer.send(REBUILD_APPLICATION_MENU);
  };

  render() {
    const { setProfileLocaleRequest, LANGUAGE_OPTIONS, currentLocale } = this.props.stores.profile;
    const isSubmitting = setProfileLocaleRequest.isExecuting;
    return (
      <GeneralSettings
        onSelectLanguage={this.onSelectLanguage}
        isSubmitting={isSubmitting}
        languages={LANGUAGE_OPTIONS}
        currentLocale={currentLocale}
        error={setProfileLocaleRequest.error}
      />
    );
  }

}
