import { action } from '@storybook/addon-actions';

export const aboutDialogProps = {
  actions: {
    app: {
      closeAboutDialog: action('closeAboutDialog'),
    },
  },
  stores: {
    app: {
      openExternalLink: action('onOpenExternalLink'),
      environment: {
        apiVersion: '2020.3.30',
        nodeVersion: '1.9.1',
        build: 'dev',
        os: 'macOS',
        version: '1.0.0',
      },
    },
  },
};
