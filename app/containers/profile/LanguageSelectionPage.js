// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import LanguageSelectionForm from '../../components/profile/language-selection/LanguageSelectionForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class LanguageSelectionPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  onSubmit = (values: { locale: string }) => {
    this.props.actions.profile.updateLocale.trigger(values);
  };

  render() {
    const { setProfileLocaleRequest, LANGUAGE_OPTIONS, currentRoute } = this.props.stores.app;
    const isSubmitting = setProfileLocaleRequest.isExecuting;
    const topbar = <TopBar currentRoute={currentRoute} />;
    return (
      <TopBarLayout
        topbar={topbar}
      >
        <LanguageSelectionForm
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          languages={LANGUAGE_OPTIONS}
          error={setProfileLocaleRequest.error}
        />
      </TopBarLayout>
    );
  }
}
