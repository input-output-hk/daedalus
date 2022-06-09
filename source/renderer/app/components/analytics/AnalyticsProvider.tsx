import React, { FC } from 'react';
import { inject, observer } from 'mobx-react';
import { AnalyticsContext } from './AnalyticsContext';
import { InjectedProps } from '../../types/injectedPropsType';

interface AnalyticsProviderProps extends InjectedProps {
  children: React.ReactNode;
}

const AnalyticsProvider: FC = inject(
  'stores',
  'actions'
)(
  observer((props: AnalyticsProviderProps) => {
    return (
      <AnalyticsContext.Provider value={props.stores.analytics.analyticsClient}>
        {props.children}
      </AnalyticsContext.Provider>
    );
  })
);

export { AnalyticsProvider };
