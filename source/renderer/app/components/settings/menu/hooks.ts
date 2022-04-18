import { useState, useEffect } from 'react';

type Props = {
  enabled: boolean;
  isOnRoute: boolean;
  onLeave: () => void;
};
export const useTriggerOnRouteLeave = ({
  enabled,
  isOnRoute,
  onLeave,
}: Props) => {
  const [wasOnRoute, setWasOnRoute] = useState(false);
  useEffect(() => {
    if (!enabled) {
      return () => {};
    }

    if (isOnRoute) {
      setWasOnRoute(true);
    }

    if (wasOnRoute && !isOnRoute) {
      onLeave();
    }

    return () => {
      if (wasOnRoute) {
        onLeave();
      }
    };
  }, [isOnRoute, wasOnRoute]);
};
