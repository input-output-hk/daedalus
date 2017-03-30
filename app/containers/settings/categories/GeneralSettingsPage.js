// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Request from '../../../stores/lib/Request';
import GeneralSettings from '../../../components/settings/categories/GeneralSettings';

@inject('stores', 'actions') @observer
export default class GeneralSettingsPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      profile: PropTypes.shape({
        updateLocale: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
    stores: PropTypes.shape({
      app: PropTypes.shape({
        setProfileLocaleRequest: PropTypes.instanceOf(Request).isRequired,
        LANGUAGE_OPTIONS: PropTypes.array.isRequired,
        currentLocale: PropTypes.string.isRequired,
      }).isRequired,
    }).isRequired,
  };

  onSelectLanguage = (values: { locale: string }) => {
    this.props.actions.profile.updateLocale(values);
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
