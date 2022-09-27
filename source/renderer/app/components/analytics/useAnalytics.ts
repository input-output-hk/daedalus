import { useContext } from 'react';
import { AnalyticsContext } from './AnalyticsContext';

export const useAnalytics = () => useContext(AnalyticsContext);
