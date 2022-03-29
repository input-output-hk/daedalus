import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import AnalyticsDialog from '../../components/profile/analytics/AnalyticsDialog';
import type { InjectedProps } from '../../types/injectedPropsType';
import { AnalyticsAcceptanceStatus } from '../../analytics/types';
import { sendPageNavigationEventOnRender } from '../../analytics/sendPageNavigationEventOnRender';

@inject('stores', 'actions')
@sendPageNavigationEventOnRender('Analytics')
@observer
class AnalyticsPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  onSubmit = async (analyticsAccepted: boolean) => {
    this.props.actions.profile.acceptAnalytics.trigger(
      analyticsAccepted
        ? AnalyticsAcceptanceStatus.ACCEPTED
        : AnalyticsAcceptanceStatus.REJECTED
    );
  };

  render() {
    const { app, networkStatus, profile } = this.props.stores;
    const { setAnalyticsAcceptanceRequest } = profile;
    const { currentRoute } = app;
    const { isShelleyActivated } = networkStatus;
    const topbar = (
      <TopBar
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        currentRoute={currentRoute}
        showSubMenuToggle={false}
        isShelleyActivated={isShelleyActivated}
      />
    );
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

export default AnalyticsPage;
