import React, { useCallback, useState } from 'react';
import { defineMessages, injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import styles from './AnalyticsDialog.scss';
import { Intl } from '../../../types/i18nTypes';
import AnalyticsForm from './AnalyticsForm';

const messages = defineMessages({
  confirmButton: {
    id: 'analytics.dialog.confirmButton',
    defaultMessage: '!!!Confirm',
    description: 'Analytics data collection confirmation button text',
  },
});

interface AnalyticsDialogProps {
  intl: Intl;
  loading: boolean;
  onConfirm: (analyticsAccepted: boolean) => void;
}

const AnalyticsDialog = ({
  intl,
  loading,
  onConfirm,
}: AnalyticsDialogProps) => {
  const [allowDataCollection, setAllowDataCollection] = useState(true);
  const handleConfirm = useCallback(() => {
    onConfirm(allowDataCollection);
  }, [onConfirm, allowDataCollection]);

  return (
    <div className={styles.component}>
      <div className={styles.centeredBox}>
        <AnalyticsForm
          showTOCSummary
          onAnalyticsAcceptanceChange={setAllowDataCollection}
          analyticsAccepted={allowDataCollection}
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

export default injectIntl(AnalyticsDialog);
