// @flow
import { defineMessages } from 'react-intl';

export default defineMessages({
  discreetModeTitle: {
    id: 'settings.security.discreetMode.title',
    defaultMessage: '!!!Discreet mode',
    description:
      'Title for the "Discreet mode" setting in the security category.',
  },
  discreetModeDescription: {
    id: 'settings.security.discreetMode.description',
    defaultMessage:
      '!!!This mode hides all sensitive data from your screen, replacing it with astericses',
    description:
      'Description for the "Discreet mode" setting in the security category.',
  },
  openInDiscreetModeTitle: {
    id: 'settings.security.openInDiscreetMode.title',
    defaultMessage: '!!!Start app in discreet mode',
    description:
      'Title for the "Open in discreet mode" setting in the security category.',
  },
  openInDiscreetModeDescription: {
    id: 'settings.security.openInDiscreetMode.description',
    defaultMessage: '!!!Daedalus will start with discreet mode ON',
    description:
      'Description for the "Open in discreet mode" setting in the security category.',
  },
});
