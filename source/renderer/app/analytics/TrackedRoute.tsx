import React, { FC, useEffect } from 'react';
import { matchPath, Route, RouteProps } from 'react-router';
import { useAnalytics } from '../components/analytics';

export type TrackedRouteProps = RouteProps & {
  pageTitle: string;
};

const TrackedRoute: FC<TrackedRouteProps> = (props) => {
  const analytics = useAnalytics();
  const { pageTitle, ...restProps } = props;

  useEffect(() => {
    const match = matchPath(window.location.hash.replace('#', ''), props);

    if (match !== null) {
      analytics.sendPageNavigationEvent(pageTitle);
    }
  }, [window.location.hash, props, pageTitle]);

  return <Route {...restProps} />;
};

export default TrackedRoute;
