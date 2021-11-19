// @flow
import React, { useState, useEffect } from 'react';

type Props = {
  enabled: boolean,
  isOnRoute: boolean,
  onLeave: () => void,
};

const TriggerOnRouteLeave = ({ enabled, isOnRoute, onLeave }: Props) => {
  if (!enabled) {
    return <></>;
  }
  const [wasOnRoute, setWasOnRoute] = useState(false);

  useEffect(() => {
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

  return <></>;
};

export default TriggerOnRouteLeave;
