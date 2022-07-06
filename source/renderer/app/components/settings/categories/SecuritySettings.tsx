import React from 'react';
import { injectIntl } from 'react-intl';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import styles from './SecuritySettings.scss';
import messages from './SecuritySettings.messages';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  intl: Intl;
  discreetMode: boolean;
  openDiscreetMode: boolean;
  onDiscreetModeToggle: () => void;
  onOpenDiscreetModeToggle: () => void;
};

function SecuritySettings({
  intl,
  discreetMode,
  openDiscreetMode,
  onDiscreetModeToggle,
  onOpenDiscreetModeToggle,
}: Props) {
  return (
    <div className={styles.root}>
      <div className={styles.setting}>
        <div className={styles.title}>
          {intl.formatMessage(messages.discreetModeTitle)}
        </div>
        <div className={styles.settingContent}>
          <p className={styles.description}>
            {intl.formatMessage(messages.discreetModeDescription)}
          </p>
          <NormalSwitch
            checked={discreetMode}
            onChange={onDiscreetModeToggle}
          />
        </div>
      </div>
      <div className={styles.setting}>
        <div className={styles.title}>
          {intl.formatMessage(messages.openInDiscreetModeTitle)}
        </div>
        <div className={styles.settingContent}>
          <p className={styles.description}>
            {intl.formatMessage(messages.openInDiscreetModeDescription)}
          </p>
          <NormalSwitch
            checked={openDiscreetMode}
            onChange={onOpenDiscreetModeToggle}
          />
        </div>
      </div>
    </div>
  );
}

export default injectIntl(SecuritySettings);
