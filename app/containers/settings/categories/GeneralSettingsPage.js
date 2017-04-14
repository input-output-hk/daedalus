// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import GeneralSettings from '../../../components/settings/categories/GeneralSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class GeneralSettingsPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  onSelectLanguage = (values: { locale: string }) => {
    this.props.actions.profile.updateLocale.trigger(values);
  };

  render() {
    const { setProfileLocaleRequest, LANGUAGE_OPTIONS, currentLocale } = this.props.stores.app;
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
