import React from 'react';
import { observer } from 'mobx-react';
import { useDiscreetModeFeature } from '../../../features/discreet-mode';
import SecuritySettings from '../../../components/settings/categories/SecuritySettings';
import { useSendPageNavigationEvent } from '../../../analytics/useSendPageNavigationEvent';
import { useAnalytics } from '../../../components/analytics';

const SecuritySettingsPage = () => {
  useAnalytics().sendPageNavigationEvent('Security Settings');

  const discreetModeFeature = useDiscreetModeFeature();

  return (
    <SecuritySettings
      discreetMode={discreetModeFeature.isDiscreetMode}
      openDiscreetMode={discreetModeFeature.openInDiscreetMode}
      onDiscreetModeToggle={discreetModeFeature.toggleDiscreetMode}
      onOpenDiscreetModeToggle={discreetModeFeature.toggleOpenInDiscreetMode}
    />
  );
};

export default observer(SecuritySettingsPage);
