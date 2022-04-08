import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import AnalyticsDialog from '../../components/profile/analytics/AnalyticsDialog';
import type { InjectedProps } from '../../types/injectedPropsType';
import { AnalyticsAcceptanceStatus } from '../../analytics/types';

@inject('stores', 'actions')
@observer
class AnalyticsConsentPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  state = {
    pageViewEventSent: false,
  };

  onSubmit = async (analyticsAccepted: boolean) => {
    await this.props.actions.profile.acceptAnalytics.trigger(
      analyticsAccepted
        ? AnalyticsAcceptanceStatus.ACCEPTED
        : AnalyticsAcceptanceStatus.REJECTED
    );
    await this.props.stores.analytics.resetAnalyticsClient();
  };

  render() {
    const { networkStatus, profile } = this.props.stores;
    const { setAnalyticsAcceptanceRequest } = profile;
    const { isShelleyActivated } = networkStatus;
    const topbar = <TopBar isShelleyActivated={isShelleyActivated} />;
    return (
      <TopBarLayout topbar={topbar}>
        <AnalyticsDialog
          loading={setAnalyticsAcceptanceRequest.isExecuting}
          onConfirm={this.onSubmit}
        />
      </TopBarLayout>
    );
  }
}

export default AnalyticsConsentPage;
