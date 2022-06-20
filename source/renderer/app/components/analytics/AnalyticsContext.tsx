import React from 'react';
import { AnalyticsClient, NoopAnalyticsClient } from '../../analytics';

export const AnalyticsContext = React.createContext<AnalyticsClient>(
  NoopAnalyticsClient
);
