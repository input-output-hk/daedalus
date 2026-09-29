import React, { useCallback } from 'react';
import { observer } from 'mobx-react';
import TopBar from '../../components/layout/TopBar';
import TopBarLayout from '../../components/layout/TopBarLayout';
import AnalyticsConsentForm from '../../components/profile/analytics/AnalyticsConsentForm';
import { AnalyticsAcceptanceStatus } from '../../analytics/types';
import { useActions } from '../../hooks/useActions';
import { useStores } from '../../hooks/useStores';

export const AnalyticsConsentPage = observer(() => {
  const actions = useActions();
  const { networkStatus, analyticsConsent } = useStores();

  const handleSubmit = useCallback(async (analyticsAccepted: boolean) => {
    await actions.profile.acceptAnalytics.trigger(
      analyticsAccepted
        ? AnalyticsAcceptanceStatus.ACCEPTED
        : AnalyticsAcceptanceStatus.REJECTED
    );
  }, []);

  const { isShelleyActivated } = networkStatus;

  const topbar = <TopBar isShelleyActivated={isShelleyActivated} />;
  return (
    <TopBarLayout topbar={topbar}>
      <AnalyticsConsentForm
        loading={analyticsConsent.saving}
        saveFailed={analyticsConsent.saveFailed}
        onSubmit={handleSubmit}
        available={!!analyticsConsent.view?.enabled}
      />
    </TopBarLayout>
  );
});

export default AnalyticsConsentPage;
