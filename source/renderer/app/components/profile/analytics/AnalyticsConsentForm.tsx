import React, { useCallback } from 'react';
import { injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import classnames from 'classnames';
import styles from './AnalyticsConsentForm.scss';
import { Intl } from '../../../types/i18nTypes';
import { messages } from './AnalyticsConsentForm.messages';
import { CollectedDataOverview } from './CollectedDataOverview';

interface AnalyticsConsentFormProps {
  intl: Intl;
  loading: boolean;
  available: boolean;
  saveFailed?: boolean;
  onSubmit: (analyticsAccepted: boolean) => void;
}

function AnalyticsConsentForm({
  intl,
  loading,
  available,
  saveFailed,
  onSubmit,
}: AnalyticsConsentFormProps) {
  const handleAllow = useCallback(() => {
    onSubmit(true);
  }, [onSubmit]);
  const handleSkip = useCallback(() => {
    onSubmit(false);
  }, [onSubmit]);

  return (
    <div className={styles.component}>
      <div className={styles.centeredBox}>
        <h2 className={styles.title}>{intl.formatMessage(messages.title)}</h2>
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>
        <CollectedDataOverview />
        <p className={styles.privacyPolicyDescription}>
          {intl.formatMessage(messages.analyticsSectionPrivacyPolicy)}
        </p>
        {!available && <p>{intl.formatMessage(messages.disabled)}</p>}
        {saveFailed && (
          <p role="alert">{intl.formatMessage(messages.saveFailed)}</p>
        )}
        <div className={styles.actions}>
          <Button
            className={classnames(styles.disallowButton, 'flat')}
            label={intl.formatMessage(messages.disallowButton)}
            skin={ButtonSpinnerSkin}
            loading={loading}
            onClick={handleSkip}
            disabled={loading}
          />
          <Button
            label={intl.formatMessage(messages.allowButton)}
            skin={ButtonSpinnerSkin}
            loading={loading}
            onClick={handleAllow}
            disabled={!available || loading}
          />
        </div>
      </div>
    </div>
  );
}

export default injectIntl(AnalyticsConsentForm);
