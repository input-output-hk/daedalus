import React from 'react';
import { defineMessages } from 'react-intl';
import styles from './AnalyticsForm.scss';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import { Intl } from '../../../types/i18nTypes';
import { CollapsibleTextBlock } from '../../widgets/collapsible-text-block/CollapsibleTextBlock';

const messages = defineMessages({
  title: {
    id: 'analytics.form.title',
    defaultMessage: '!!!Anonymous data collection',
    description: 'Analytics form title',
  },
  description: {
    id: 'analytics.form.description',
    // TODO Terms and Conditions link
    defaultMessage:
      '!!!All data is anonymous and is only used for product development purposes. Read more in the {termsAndConditionsLink}.',
    description: 'Analytics data collection description',
  },
  dataCollectionDetailsTitle: {
    id: 'analytics.form.dataCollectionDetailsTitle',
    defaultMessage: '!!!What data do we collect?',
    description: 'Data collection details title',
  },
  dataCollectionDetailsUserBehaviour: {
    id: 'analytics.form.dataCollectionDetailsUserBehaviour',
    defaultMessage: '!!!User behavior (where the user clicks)',
    description: 'Description for the user behaviour data collection',
  },
  dataCollectionDetailsDeviceInfo: {
    id: 'analytics.form.dataCollectionDetailsDeviceInfo',
    defaultMessage: '!!!Device information (OS, RAM, disk space, etc)',
    description: 'Description for the device info data collection',
  },
  dataCollectionSwitchButton: {
    id: 'analytics.form.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
});

interface AnalyticsFormProps {
  intl: Intl;
  onAnalyticsAcceptanceChange: (analyticsAccepted: boolean) => void;
  analyticsAccepted: boolean;
}

const AnalyticsForm = ({
  intl,
  onAnalyticsAcceptanceChange,
  analyticsAccepted,
}: AnalyticsFormProps) => {
  return (
    <>
      <p className={styles.title}>{intl.formatMessage(messages.title)}</p>
      <p className={styles.description}>
        {intl.formatMessage(messages.description)}
      </p>
      <CollapsibleTextBlock
        header={intl.formatMessage(messages.dataCollectionDetailsTitle)}
      >
        <ol className={styles.dataCollectionList}>
          <li className={styles.dataCollectionListItem}>
            {intl.formatMessage(messages.dataCollectionDetailsUserBehaviour)}
          </li>
          <li className={styles.dataCollectionListItem}>
            {intl.formatMessage(messages.dataCollectionDetailsDeviceInfo)}
          </li>
        </ol>
        {/* <hr className={styles.hr} /> */}
      </CollapsibleTextBlock>
      <NormalSwitch
        onChange={onAnalyticsAcceptanceChange}
        checked={analyticsAccepted}
        label={intl.formatMessage(messages.dataCollectionSwitchButton)}
        className={styles.switchButton}
      />
    </>
  );
};

export default AnalyticsForm;
