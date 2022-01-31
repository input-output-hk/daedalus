// @flow
import React, { useCallback, useState } from 'react';
import { defineMessages, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import styles from './AnalyticsDialog.scss';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import { Intl } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
  title: {
    id: 'analytics.dialog.title',
    defaultMessage: '!!!Anonymous data collection',
    description: 'Analytics dialog title',
  },
  description: {
    id: 'analytics.dialog.description',
    defaultMessage:
      // TODO Terms and Conditions link
      '!!!All data is anonymous and is only used for product development purposes. Read more in the {termsAndConditionsLink}.',
    description: 'Analytics data collection description',
  },
  dataCollectionDetailsTitle: {
    id: 'analytics.dialog.dataCollectionDetailsTitle',
    defaultMessage: '!!!What data do we collect?',
    description: 'Data collection details title',
  },
  dataCollectionDetailsUserBehaviour: {
    id: 'analytics.dialog.dataCollectionDetailsUserBehaviour',
    defaultMessage: '!!!User behavior (where the user clicks)',
    description: 'Description for the user behaviour data collection',
  },
  dataCollectionDetailsDeviceInfo: {
    id: 'analytics.dialog.dataCollectionDetailsDeviceInfo',
    defaultMessage: '!!!Device information (OS, RAM, disk space, etc)',
    description: 'Description for the device info data collection',
  },
  dataCollectionSwitchButton: {
    id: 'analytics.dialog.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
  confirmButton: {
    id: 'analytics.dialog.confirmButton',
    defaultMessage: '!!!Confirm',
    description: 'Analytics data collection confirmation button text',
  },
});

type Props = {
  intl: Intl,
  loading: boolean,
  onConfirm: Function,
};

const AnalyticsDialog = ({ intl, loading, onConfirm }: Props) => {
  const [showDataCollectionDetails, setShowDataCollectionDetails] = useState(
    false
  );
  const toggleShowDataCollectionDetails = useCallback(() => {
    setShowDataCollectionDetails(
      (prevAllowDataCollection) => !prevAllowDataCollection
    );
  }, [setShowDataCollectionDetails]);
  const [allowDataCollection, setAllowDataCollection] = useState(true);
  const toggleAllowDataCollection = useCallback(() => {
    setAllowDataCollection(
      (prevAllowDataCollection) => !prevAllowDataCollection
    );
  }, [setAllowDataCollection]);
  const getShowDataCollectionDetailsToggleLabel = useCallback(
    (isVisible: boolean) =>
      isVisible
        ? intl.formatMessage(globalMessages.hide)
        : intl.formatMessage(globalMessages.view)
  );

  return (
    <div className={styles.component}>
      <div className={styles.centeredBox}>
        <p className={styles.title}>{intl.formatMessage(messages.title)}</p>
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>
        <p className={styles.dataCollectionTitle}>
          {intl.formatMessage(messages.dataCollectionDetailsTitle)}
          <button
            className={styles.toggleButton}
            onClick={toggleShowDataCollectionDetails}
          >
            {getShowDataCollectionDetailsToggleLabel(showDataCollectionDetails)}
          </button>
        </p>
        {showDataCollectionDetails && (
          <>
            <ol className={styles.dataCollectionList}>
              <li className={styles.dataCollectionListItem}>
                {intl.formatMessage(
                  messages.dataCollectionDetailsUserBehaviour
                )}
              </li>
              <li className={styles.dataCollectionListItem}>
                {intl.formatMessage(messages.dataCollectionDetailsDeviceInfo)}
              </li>
            </ol>
            <hr className={styles.hr} />
          </>
        )}
        <NormalSwitch
          onChange={toggleAllowDataCollection}
          checked={allowDataCollection}
          label={intl.formatMessage(messages.dataCollectionSwitchButton)}
          className={styles.switchButton}
        />
        <Button
          className={styles.submitButton}
          label={intl.formatMessage(messages.confirmButton)}
          skin={ButtonSpinnerSkin}
          loading={loading}
          onClick={onConfirm}
        />
      </div>
    </div>
  );
};

export default injectIntl(AnalyticsDialog);
