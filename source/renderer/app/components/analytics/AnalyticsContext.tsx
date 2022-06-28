import React from 'react';
import { AnalyticsTracker } from '../../analytics';

export const AnalyticsContext = React.createContext<AnalyticsTracker>(null);
