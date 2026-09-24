import {
  ARIADNE_ANALYTICS_CONSENT,
  ARIADNE_ANALYTICS_EVENT,
  ConsentCommand,
  ConsentView,
  EventMessage,
} from '../../../common/ipc/api';

export const analyticsConsent = (
  command: ConsentCommand
): Promise<ConsentView> =>
  global.ipcRenderer.invoke(ARIADNE_ANALYTICS_CONSENT, command);
export const analyticsEvent = (event: EventMessage): Promise<boolean> =>
  global.ipcRenderer.invoke(ARIADNE_ANALYTICS_EVENT, event);
