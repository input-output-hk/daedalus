import React, { FC, useCallback, useState } from 'react';
import { injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import styles from './AnalyticsConsentForm.scss';
import { Intl } from '../../../types/i18nTypes';
import { CollapsibleSection } from '../../widgets/collapsible-section/CollapsibleSection';
import { MonospaceTextBlock } from '../../widgets/monospace-text-block/MonospaceTextBlock';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import { messages } from './AnalyticsConsentForm.messages';
import { CollectedDataOverview } from './CollectedDataOverview';

interface AnalyticsConsentFormProps {
  intl: Intl;
  loading: boolean;
  onConfirm: (analyticsAccepted: boolean) => void;
}

const AnalyticsConsentForm: FC<AnalyticsConsentFormProps> = ({
  intl,
  loading,
  onConfirm,
}: AnalyticsConsentFormProps) => {
  const [accepted, setAccepted] = useState(true);

  const handleConfirm = useCallback(() => {
    onConfirm(accepted);
  }, [onConfirm, accepted]);

  return (
    <div className={styles.component}>
      <div className={styles.centeredBox}>
        <h2 className={styles.title}>{intl.formatMessage(messages.title)}</h2>
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>
        <CollapsibleSection
          header={intl.formatMessage(messages.tocDetailsTitle)}
        >
          <MonospaceTextBlock>
            {intl.formatMessage(messages.tocDetails)}
          </MonospaceTextBlock>
        </CollapsibleSection>
        <CollectedDataOverview displaySeparatorUnderneath />
        <NormalSwitch
          onChange={setAccepted}
          checked={accepted}
          label={intl.formatMessage(messages.dataCollectionSwitchButton)}
          className={styles.switchButton}
        />
        <Button
          className={styles.submitButton}
          label={intl.formatMessage(messages.confirmButton)}
          skin={ButtonSpinnerSkin}
          loading={loading}
          onClick={handleConfirm}
        />
      </div>
    </div>
  );
};

export default injectIntl(AnalyticsConsentForm);
