// @flow
import React, { PropTypes, Component } from 'react';
import { inject, observer } from 'mobx-react';
import CachedRequest from '../../stores/lib/CachedRequest';
import CenteredLayout from '../../components/layout/CenteredLayout';
import LanguageSelectionForm from '../../components/profile/language-selection/LanguageSelectionForm';

@inject('stores', 'actions') @observer
export default class LanguageSelectionPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      profile: PropTypes.shape({
        updateLocale: PropTypes.func.isRequired,
      }),
    }),
    stores: PropTypes.shape({
      app: PropTypes.shape({
        profileLocaleRequest: PropTypes.instanceOf(CachedRequest).isRequired,
        LANGUAGE_OPTIONS: PropTypes.array.isRequired,
      }).isRequired,
    }).isRequired,
  };

  onSubmit = (values: { locale: string }) => {
    this.props.actions.profile.updateLocale(values);
  };

  render() {
    const { profileLocaleRequest, LANGUAGE_OPTIONS } = this.props.stores.app;
    const isSubmitting = profileLocaleRequest.isExecuting;
    return (
      <CenteredLayout>
        <LanguageSelectionForm
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          languages={LANGUAGE_OPTIONS}
        />
      </CenteredLayout>
    );
  }
}
