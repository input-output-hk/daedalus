import React from 'react';
import { boolean, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import SystemTimeError from '../../../../source/renderer/app/components/loading/system-time-error/SystemTimeError';

export const SystemTimeErrorStory = ({ locale }: { locale: string }) => (
  <SystemTimeError
    localTimeDifference={number('localTimeDifference', 0)}
    currentLocale={locale}
    onExternalLinkClick={action('onExternalLinkClick')}
    onCheckTheTimeAgain={action('onExternalLinkClick')}
    onContinueWithoutClockSyncCheck={action('onExternalLinkClick')}
    isCheckingSystemTime={boolean('isCheckingSystemTime', false)}
  />
);
