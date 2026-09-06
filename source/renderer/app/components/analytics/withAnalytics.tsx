import React from 'react';
import { AnalyticsTracker } from '../../analytics';
import { useAnalytics } from './useAnalytics';

export interface WithAnalyticsTrackerProps {
  analyticsTracker: AnalyticsTracker;
}

export function withAnalytics<
  T extends WithAnalyticsTrackerProps = WithAnalyticsTrackerProps
>(WrappedComponent: React.ComponentType<T>) {
  const displayName =
    WrappedComponent.displayName || WrappedComponent.name || 'Component';

  function ComponentWithTheme(props: Omit<T, keyof WithAnalyticsTrackerProps>) {
    const analyticsTracker = useAnalytics();

    // props comes afterwards so the can override the default ones.
    return (
      <WrappedComponent {...(props as T)} analyticsTracker={analyticsTracker} />
    );
  }

  ComponentWithTheme.displayName = `withAnalytics(${displayName})`;

  return ComponentWithTheme;
}
