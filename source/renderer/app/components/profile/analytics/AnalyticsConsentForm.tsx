import React, { useCallback } from 'react';
import { FormattedMessage, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Link } from 'react-polymorph/lib/components/Link';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import classnames from 'classnames';
import styles from './AnalyticsConsentForm.scss';
import { Intl } from '../../../types/i18nTypes';
import { messages } from './AnalyticsConsentForm.messages';
import { CollectedDataOverview } from './CollectedDataOverview';
import { PRIVACY_POLICY_LINK } from '../../../config/analyticsConfig';

interface AnalyticsConsentFormProps {
  intl: Intl;
  loading: boolean;
  onSubmit: (analyticsAccepted: boolean) => void;
  onExternalLinkClick: (url: string) => void;
}

function AnalyticsConsentForm({
  intl,
  loading,
  onSubmit,
  onExternalLinkClick,
}: AnalyticsConsentFormProps) {
  const handleAllow = useCallback(() => {
    onSubmit(true);
  }, []);
  const handleSkip = useCallback(() => {
    onSubmit(false);
  }, []);

  const privacyPolicyLink = (
    <Link
      className={styles.privacyPolicyLink}
      onClick={() => onExternalLinkClick(PRIVACY_POLICY_LINK)}
      label={intl.formatMessage(messages.privacyPolicyLink)}
      hasIconAfter={false}
    />
  );

  return (
    <div className={styles.component}>
      <div className={styles.centeredBox}>
        <h2 className={styles.title}>{intl.formatMessage(messages.title)}</h2>
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>
        <CollectedDataOverview />
        <p className={styles.privacyPolicyDescription}>
          <FormattedMessage
            {...messages.analyticsSectionPrivacyPolicy}
            values={{
              privacyPolicyLink,
            }}
          />
        </p>
        <div className={styles.actions}>
          <Button
            className={classnames(styles.disallowButton, 'flat')}
            label={intl.formatMessage(messages.disallowButton)}
            skin={ButtonSpinnerSkin}
            loading={loading}
            onClick={handleSkip}
          />
          <Button
            label={intl.formatMessage(messages.allowButton)}
            skin={ButtonSpinnerSkin}
            loading={loading}
            onClick={handleAllow}
          />
        </div>
      </div>
    </div>
  );
}

export default injectIntl(AnalyticsConsentForm);
