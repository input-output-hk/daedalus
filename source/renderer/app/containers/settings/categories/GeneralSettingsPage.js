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

  onSelectLanguage = async (values: { locale: string }) => {
    const { actions, stores } = this.props;
    const { isUpdateAvailable } = stores.nodeUpdate;
    const { updateLocale } = actions.profile;
    updateLocale.trigger(values);
    await rebuildApplicationMenu.send({ isUpdateAvailable });
  };

  render() {
    const {
      setProfileLocaleRequest,
      LANGUAGE_OPTIONS,
      currentLocale,
    } = this.props.stores.profile;
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
