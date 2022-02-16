import React from 'react';
import { injectIntl } from 'react-intl';
import { DiscreetModeToggle } from '../../../features/discreet-mode-poc/ui/settings/DiscreetModeToggle';
import { OpenInDiscreetModeToggle } from '../../../features/discreet-mode-poc/ui/settings/OpenInDiscreetModeToggle';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SecuritySettings.scss' or it... Remove this comment to see the full error message
import styles from './SecuritySettings.scss';
import messages from './SecuritySettings.messages';
import type { Intl } from '../../../types/i18nTypes';

type Props = {
  intl: Intl;
};

const SecuritySettings = ({ intl }: Props) => {
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
          <DiscreetModeToggle />
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
          <OpenInDiscreetModeToggle />
        </div>
      </div>
    </div>
  );
};

export default injectIntl(SecuritySettings);
