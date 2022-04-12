import React, { FC } from 'react';
import { defineMessages, injectIntl } from 'react-intl';
import { CollapsibleSection } from '../../widgets/collapsible-section/CollapsibleSection';
import styles from './CollectedDataOverview.scss';
import { Separator } from '../../widgets/separator/Separator';

const messages = defineMessages({
  title: {
    id: 'analytics.form.dataCollectionDetailsTitle',
    defaultMessage: '!!!What data do we collect?',
    description: 'Data collection details title',
  },
  userBehaviour: {
    id: 'analytics.form.dataCollectionDetailsUserBehaviour',
    defaultMessage: '!!!User behavior (where the user clicks)',
    description: 'Description for the user behaviour data collection',
  },
  deviceInfo: {
    id: 'analytics.form.dataCollectionDetailsDeviceInfo',
    defaultMessage: '!!!Device information (OS, RAM, disk space, etc)',
    description: 'Description for the device info data collection',
  },
});

interface CollectedDataOverviewProps {
  displaySeparatorUnderneath?: boolean;
}

export const CollectedDataOverview: FC<CollectedDataOverviewProps> = injectIntl(
  ({ intl, displaySeparatorUnderneath }) => (
    <CollapsibleSection header={intl.formatMessage(messages.title)}>
      <ol className={styles.dataCollectionList}>
        <li>{intl.formatMessage(messages.userBehaviour)}</li>
        <li>{intl.formatMessage(messages.deviceInfo)}</li>
      </ol>
      {displaySeparatorUnderneath && <Separator />}
    </CollapsibleSection>
  )
);
