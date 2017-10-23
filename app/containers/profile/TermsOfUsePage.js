// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import TermsOfUseForm from '../../components/profile/terms-of-use/TermsOfUseForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class TermsOfUsePage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  onSubmit = () => {
    this.props.actions.profile.acceptTermsOfUse.trigger();
  };

  render() {
    const { setTermsOfUseAcceptanceRequest, termsOfUse } = this.props.stores.profile;
    const { currentRoute } = this.props.stores.app;
    const isSubmitting = setTermsOfUseAcceptanceRequest.isExecuting;
    const topbar = <TopBar currentRoute={currentRoute} />;
    return (
      <TopBarLayout
        topbar={topbar}
      >
        <TermsOfUseForm
          localizedTermsOfUse={termsOfUse}
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          error={setTermsOfUseAcceptanceRequest.error}
        />
      </TopBarLayout>
    );
  }
}
