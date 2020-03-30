// @flow
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
        apiVersion: 'dev',
        nodeVersion: '1.9.1',
        build: 'dev',
        os: 'macOS',
        version: '0.15.0',
      },
    },
  },
};
