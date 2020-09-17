// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { number, withKnobs, radios, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import AppUpdateOverlay from '../../../source/renderer/app/components/appUpdate/AppUpdateOverlay';
import { update, version, availableAppVersion } from './_utils/fakeDataUpdate';
import { rangeMap } from '../../../source/renderer/app/utils/numbers';

storiesOf('News|Overlays', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  .add('Update', ({ locale }: { locale: string }) => {
    const scenario = radios(
      'Scenario',
      {
        Downloading: 'downloading',
        'Download complete': 'downloaded',
        'Download complete. User reopened app without installing the update':
          'reopened',
        'Process failed': 'failed',
      },
      'downloading'
    );

    let isUpdateDownloaded = true;
    let isAutomaticUpdateFailed = false;
    let displayManualUpdateLink = false;

    if (scenario === 'downloading') {
      isUpdateDownloaded = false;
    } else if (scenario === 'reopened') {
      displayManualUpdateLink = true;
    } else if (scenario === 'failed') {
      isAutomaticUpdateFailed = true;
    }

    const downloadProgress =
      scenario === 'downloading'
        ? number('downloadProgress', 30, {
            range: true,
            min: 0,
            max: 100,
            step: 1,
          })
        : 0;

    const timeLeftNumber = parseInt(
      rangeMap(downloadProgress, 0, 100, 30, 1),
      10
    );

    const downloadTimeLeft = {
      'EN-US': `${timeLeftNumber} minutes`,
      'JP-JP': `${timeLeftNumber}分`,
    };

    const isLinux =
      scenario === 'downloaded' || scenario === 'reopened'
        ? boolean('isLinux', false)
        : false;

    const isInstallingUpdate = isLinux
      ? boolean('isInstallingUpdate', false)
      : false;

    return (
      <AppUpdateOverlay
        update={update[locale]}
        downloadTimeLeft={downloadTimeLeft[locale]}
        totalDownloaded="10Mb"
        totalDownloadSize="30Mb"
        availableAppVersion={availableAppVersion}
        currentAppVersion={version}
        downloadProgress={downloadProgress}
        isUpdateDownloaded={isUpdateDownloaded}
        isAutomaticUpdateFailed={isAutomaticUpdateFailed}
        displayManualUpdateLink={displayManualUpdateLink}
        onClose={action('onClose')}
        onInstallUpdate={action('onInstallUpdate')}
        onPostponeUpdate={action('onPostponeUpdate')}
        onExternalLinkClick={action('onExternalLinkClick')}
        isInstallingUpdate={isInstallingUpdate}
        isLinux={isLinux}
      />
    );
  });
