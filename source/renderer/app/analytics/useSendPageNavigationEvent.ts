import { useStores } from '../hooks/useStores';

export const useSendPageNavigationEvent = (pageTitle: string) => {
  const { analytics } = useStores();
  analytics.analyticsClient.sendPageNavigationEvent(pageTitle);
};
