// @flow
import React, { PropTypes, Component } from 'react';
import { inject, observer } from 'mobx-react';
import Request from '../../stores/lib/Request';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
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
        setProfileLocaleRequest: PropTypes.instanceOf(Request).isRequired,
        LANGUAGE_OPTIONS: PropTypes.array.isRequired,
      }).isRequired,
    }).isRequired,
  };

  onSubmit = (values: { locale: string }) => {
    this.props.actions.profile.updateLocale(values);
  };

  render() {
    const { setProfileLocaleRequest, LANGUAGE_OPTIONS } = this.props.stores.app;
    const isSubmitting = setProfileLocaleRequest.isExecuting;
    const topbar = <TopBar />;
    return (
      <TopBarLayout
        topbar={topbar}
      >
        <LanguageSelectionForm
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          languages={LANGUAGE_OPTIONS}
        />
      </TopBarLayout>
    );
  }
}
