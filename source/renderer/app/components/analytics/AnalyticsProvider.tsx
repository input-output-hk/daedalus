import React, { FC } from 'react';
import { AnalyticsContext } from './AnalyticsContext';
import { AnalyticsTracker } from '../../analytics';

interface AnalyticsProviderProps {
  children: React.ReactNode;
  tracker: AnalyticsTracker;
}

function AnalyticsProvider({ children, tracker }: AnalyticsProviderProps) {
  return (
    <AnalyticsContext.Provider value={tracker}>
      {children}
    </AnalyticsContext.Provider>
  );
}

export { AnalyticsProvider };
