// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { number, withKnobs, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import AppUpdateOverlay from '../../../source/renderer/app/components/appUpdate/AppUpdateOverlay';
import { update, version, availableAppVersion } from './_utils/fakeDataUpdate';
import { rangeMap } from '../../../source/renderer/app/utils/numbers';

storiesOf('News|Overlays', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  .add('Update', ({ locale }: { locale: string }) => {
    const downloadProgress = number('downloadProgress', 30, {
      range: true,
      min: 0,
      max: 100,
      step: 1,
    });
    const timeLeftNumber = parseInt(
      rangeMap(downloadProgress, 0, 100, 30, 1),
      10
    );
    const downloadTimeLeft = `${timeLeftNumber} minutes`;
    return (
      <AppUpdateOverlay
        update={update[locale]}
        downloadTimeLeft={downloadTimeLeft}
        totalDownloaded="10Mb"
        totalDownloadSize="30Mb"
        availableAppVersion={availableAppVersion}
        currentAppVersion={version}
        downloadProgress={downloadProgress}
        isUpdateDownloaded={boolean('isUpdateDownloaded', true)}
        isAutomaticUpdateFailed={boolean('isAutomaticUpdateFailed', false)}
        displayManualUpdateLink={boolean('displayManualUpdateLink', true)}
        onClose={action('onClose')}
        onInstallUpdate={action('onInstallUpdate')}
        onExternalLinkClick={action('onExternalLinkClick')}
      />
    );
  });
