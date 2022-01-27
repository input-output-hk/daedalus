import React from 'react';
import { storiesOf } from '@storybook/react';
import { number, withKnobs, radios, boolean } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../_support/StoryDecorator';
import AppUpdateOverlay from '../../../source/renderer/app/components/appUpdate/AppUpdateOverlay';
import { update, version, availableAppVersion } from './_utils/fakeDataUpdate';
import { rangeMap } from '../../../source/renderer/app/utils/numbers';

storiesOf('News|Overlays', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('Update', ({ locale }: { locale: string }) => {
    const scenario = radios(
      'Scenario',
      {
        Downloading: 'downloading',
        'Download complete': 'downloaded',
        'Process failed': 'failed',
      },
      'downloading'
    );
    let isUpdateDownloaded = boolean('isUpdateDownloaded', true);
    let isAutomaticUpdateFailed = false;
    let isLinux = boolean('isLinux', false);
    const isFlight = boolean('isFlight', false);
    const isTestnet = boolean('isTestnet', false);
    let isWaitingToQuitDaedalus = boolean('isWaitingToQuitDaedalus', false);
    let installationProgress = 0;

    if (scenario === 'downloading') {
      isUpdateDownloaded = false;
    } else if (scenario === 'failed') {
      isAutomaticUpdateFailed = true;
    } else if (scenario === 'downloaded') {
      isLinux = boolean('isLinux', false);
      isWaitingToQuitDaedalus = boolean('isWaitingToQuitDaedalus', false);
      if (isLinux && isWaitingToQuitDaedalus)
        installationProgress = number('installationProgress', 30, {
          range: true,
          min: 0,
          max: 100,
          step: 1,
        });
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
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
      rangeMap(downloadProgress, 0, 100, 30, 1),
      10
    );
    const downloadTimeLeft = {
      'EN-US': `${timeLeftNumber} minutes`,
      'JP-JP': `${timeLeftNumber}分`,
    };
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
        onClose={action('onClose')}
        onInstallUpdate={action('onInstallUpdate')}
        onPostponeUpdate={action('onPostponeUpdate')}
        onExternalLinkClick={action('onExternalLinkClick')}
        isWaitingToQuitDaedalus={isWaitingToQuitDaedalus}
        isLinux={isLinux}
        isFlight={isFlight}
        isTestnet={isTestnet}
        installationProgress={installationProgress}
      />
    );
  });
