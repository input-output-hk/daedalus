import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  on: {
    id: 'discreetMode.discreetToggle.on',
    defaultMessage: '!!!Toggle discreet mode on.',
    description:
      'Text for the tooltip on "discreet mode" button when mode is on',
  },
  off: {
    id: 'discreetMode.discreetToggle.off',
    defaultMessage: '!!!Toggle discreet mode off.',
    description:
      'Text for the tooltip on "discreet mode" button when mode is off',
  },
  description: {
    id: 'discreetMode.discreetToggle.description',
    defaultMessage: '!!!You can toggle auto discreet mode in <b>Settings</b>.',
    description: 'Text for the tooltip on "discreet mode" button description',
  },
});
