import React, { ReactNode, useCallback, useState } from 'react';
import { injectIntl } from 'react-intl';
import styles from './CollapsibleSection.scss';
import { Intl } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';

interface CollapsibleSectionProps {
  intl: Intl;
  header: string;
  children: ReactNode;
}

export const CollapsibleSection = injectIntl(
  ({ intl, header, children }: CollapsibleSectionProps) => {
    const [expanded, setExpanded] = useState(false);
    const handleToggle = useCallback(() => {
      setExpanded((previousExpanded) => !previousExpanded);
    }, [setExpanded]);

    return (
      <>
        <h2 className={styles.header}>
          {header}
          <button className={styles.toggleButton} onClick={handleToggle}>
            {intl.formatMessage(
              expanded ? globalMessages.hide : globalMessages.view
            )}
          </button>
        </h2>
        {expanded && children}
      </>
    );
  }
);
