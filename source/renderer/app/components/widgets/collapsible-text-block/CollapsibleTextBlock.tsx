import React, { ReactNode, useCallback, useState } from 'react';
import { injectIntl } from 'react-intl';
import styles from './CollapsibleTextBlock.scss';
import { Intl } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';

interface CollapsibleTextBlockProps {
  intl: Intl;
  header: string;
  children: ReactNode;
}

export const CollapsibleTextBlock = injectIntl(
  ({ intl, header, children }: CollapsibleTextBlockProps) => {
    const [expanded, setExpanded] = useState(false);
    const handleToggle = useCallback(() => {
      setExpanded((previousExpanded) => !previousExpanded);
    }, [setExpanded]);

    return (
      <>
        <p className={styles.header}>
          {header}
          <button className={styles.toggleButton} onClick={handleToggle}>
            {intl.formatMessage(
              expanded ? globalMessages.hide : globalMessages.view
            )}
          </button>
        </p>
        {expanded && children}
      </>
    );
  }
);
