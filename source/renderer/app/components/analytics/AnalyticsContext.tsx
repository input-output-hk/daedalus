import React from 'react';
import { AnalyticsClient } from '../../analytics';

export const AnalyticsContext = React.createContext<AnalyticsClient>(null);
