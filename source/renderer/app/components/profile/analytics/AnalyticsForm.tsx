import React, { useCallback } from 'react';
import { FormattedMessage, injectIntl } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import styles from './AnalyticsForm.scss';
import { Intl } from '../../../types/i18nTypes';
import { messages } from './AnalyticsForm.messages';
import { ROUTES } from '../../../routes-config';
import { useActions } from '../../../hooks/useActions';
import { CollapsibleTextBlock } from '../../widgets/collapsible-text-block/CollapsibleTextBlock';
import { Separator } from '../../widgets/separator/Separator';
import NormalSwitch from '../../widgets/forms/NormalSwitch';

interface AnalyticsFormProps {
  intl: Intl;
  onAnalyticsAcceptanceChange: (analyticsAccepted: boolean) => void;
  analyticsAccepted: boolean;
  showTOCSummary?: boolean;
}

const AnalyticsForm = ({
  intl,
  onAnalyticsAcceptanceChange,
  analyticsAccepted,
  showTOCSummary,
}: AnalyticsFormProps) => {
  const actions = useActions();
  const handleTOCNavigation = useCallback(() => {
    actions.router.goToRoute.trigger({
      route: ROUTES.SETTINGS.TERMS_OF_USE,
    });
  }, [actions]);

  const termsAndConditionsLink = (
    <Link
      className={styles.link}
      onClick={handleTOCNavigation}
      label={intl.formatMessage(messages.tocLink)}
      hasIconAfter={false}
    />
  );

  return (
    <>
      <h2 className={styles.title}>{intl.formatMessage(messages.title)}</h2>
      {!showTOCSummary && (
        <p className={styles.description}>
          <FormattedMessage
            {...messages.descriptionWithTOCLink}
            values={{
              termsAndConditionsLink,
            }}
          />
        </p>
      )}
      {showTOCSummary && (
        <>
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>
          <CollapsibleTextBlock
            header={intl.formatMessage(messages.tocDetailsTitle)}
          >
            <div className={styles.tocDetails}>
              {intl.formatMessage(messages.tocDetails)}
            </div>
          </CollapsibleTextBlock>
        </>
      )}
      <CollapsibleTextBlock
        header={intl.formatMessage(messages.dataCollectionDetailsTitle)}
      >
        <ol className={styles.dataCollectionList}>
          <li>
            {intl.formatMessage(messages.dataCollectionDetailsUserBehaviour)}
          </li>
          <li>
            {intl.formatMessage(messages.dataCollectionDetailsDeviceInfo)}
          </li>
        </ol>
        <Separator />
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

export default injectIntl(AnalyticsForm);
