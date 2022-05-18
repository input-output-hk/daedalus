import React, { ReactNode, useCallback, useState } from 'react';
import { injectIntl } from 'react-intl';
import classnames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import styles from './CollapsibleSection.scss';
import { Intl, ReactIntlMessage } from '../../../types/i18nTypes';
import globalMessages from '../../../i18n/global-messages';

interface CollapsibleSectionProps {
  intl: Intl;
  header: string;
  children: ReactNode;
  expandMessage?: ReactIntlMessage;
  collapseMessage?: ReactIntlMessage;
  headerFontStyle?: 'light' | 'bold';
  expandButtonStyle?: 'button' | 'link';
}

export const CollapsibleSection = injectIntl(
  ({
    intl,
    header,
    children,
    expandMessage = globalMessages.view,
    collapseMessage,
    headerFontStyle = 'bold',
    expandButtonStyle = 'button',
  }: CollapsibleSectionProps) => {
    const [expanded, setExpanded] = useState(false);
    const handleToggle = useCallback(() => {
      setExpanded((previousExpanded) => !previousExpanded);
    }, [setExpanded]);

    const buttonMessage = intl.formatMessage(
      expanded ? collapseMessage || globalMessages.hide : expandMessage
    );

    return (
      <>
        <h2
          className={classnames(
            styles.header,
            headerFontStyle === 'light'
              ? styles.lightHeader
              : styles.boldHeader,
            expandButtonStyle === 'button' && styles.flexHeader
          )}
        >
          {header}
          {expandButtonStyle === 'button' && (
            <button className={styles.toggleButton} onClick={handleToggle}>
              {buttonMessage}
            </button>
          )}
          {expandButtonStyle === 'link' && (
            <>
              {' '}
              <Link
                className={styles.toggleLink}
                onClick={handleToggle}
                label={buttonMessage}
                hasIconAfter={false}
              />
              .
            </>
          )}
        </h2>
        {expanded && children}
      </>
    );
  }
);
