// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import TermsOfUseForm from '../../components/profile/terms-of-use/TermsOfUseForm';
import { enterInitialSettingsChannel } from '../../ipc/enterInitialSettingsChannel';
import { exitFromInitialSettingsChannel } from '../../ipc/exitFromInitialSettingsChannel';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class TermsOfUsePage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  async componentDidMount() {
    await enterInitialSettingsChannel.send();
  }

  async componentWillUnmount() {
    await exitFromInitialSettingsChannel.send();
  }

  onSubmit = () => {
    this.props.actions.profile.acceptTermsOfUse.trigger();
  };

  render() {
    const { app, networkStatus, profile } = this.props.stores;
    const { setTermsOfUseAcceptanceRequest, termsOfUse } = profile;
    const { currentRoute, openExternalLink } = app;
    const { isShelleyActivated } = networkStatus;
    const isSubmitting = setTermsOfUseAcceptanceRequest.isExecuting;
    const topbar = (
      <TopBar
        currentRoute={currentRoute}
        showSubMenuToggle={false}
        isShelleyActivated={isShelleyActivated}
      />
    );

    return (
      <TopBarLayout topbar={topbar}>
        <TermsOfUseForm
          localizedTermsOfUse={termsOfUse}
          onSubmit={this.onSubmit}
          isSubmitting={isSubmitting}
          error={setTermsOfUseAcceptanceRequest.error}
          onOpenExternalLink={openExternalLink}
        />
      </TopBarLayout>
    );
  }
}
