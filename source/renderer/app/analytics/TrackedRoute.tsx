import React, { FC, useEffect } from 'react';
import { matchPath, Route, RouteProps } from 'react-router';
import { useAnalytics } from '../components/analytics';

export type TrackedRouteProps = RouteProps & {
  pageTitle: string;
};

function TrackedRoute(props: TrackedRouteProps) {
  const analytics = useAnalytics();
  const { pageTitle, ...restProps } = props;

  useEffect(() => {
    const match = matchPath(window.location.hash.replace('#', ''), props);

    if (match !== null) {
      analytics.sendPageNavigationEvent(pageTitle);
    }
  }, [window.location.hash, props]);

  return <Route {...restProps} />;
}

export default TrackedRoute;
