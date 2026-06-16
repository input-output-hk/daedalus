import React, { useState, useMemo } from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepCard from './DRepCard';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDirectoryList.scss';

const CARDS_PER_PAGE = 25;

const messages = defineMessages({
  previous: {
    id: 'governance.drepDirectory.pagination.previous',
    defaultMessage: '!!!Previous',
    description: 'Previous page button label',
  },
  next: {
    id: 'governance.drepDirectory.pagination.next',
    defaultMessage: '!!!Next',
    description: 'Next page button label',
  },
  pageInfo: {
    id: 'governance.drepDirectory.pagination.pageInfo',
    defaultMessage: '!!!Page {current} of {total}',
    description: 'Current page info',
  },
});

interface Props {
  entries: AppDRepDirectoryEntry[];
  intl: intlShape.isRequired;
}

function DRepDirectoryList({ entries, intl }: Props) {
  const [page, setPage] = useState(0);

  const totalPages = Math.max(1, Math.ceil(entries.length / CARDS_PER_PAGE));

  // Reset page if entries change and current page is out of bounds
  const safePage = useMemo(() => {
    if (page >= totalPages) {
      return 0;
    }
    return page;
  }, [page, totalPages]);

  const pageEntries = useMemo(
    () =>
      entries.slice(safePage * CARDS_PER_PAGE, (safePage + 1) * CARDS_PER_PAGE),
    [entries, safePage]
  );

  const handlePrevious = () => {
    setPage(Math.max(0, safePage - 1));
  };

  const handleNext = () => {
    setPage(Math.min(totalPages - 1, safePage + 1));
  };

  return (
    <div className={styles.container}>
      <div className={styles.list}>
        {pageEntries.map((entry) => (
          <DRepCard key={entry.drepId} entry={entry} />
        ))}
      </div>
      {totalPages > 1 && (
        <div className={styles.pagination}>
          <Button
            label={intl.formatMessage(messages.previous)}
            onClick={handlePrevious}
            disabled={safePage === 0}
            skin={ButtonSkin}
          />
          <span className={styles.pageInfo}>
            {intl.formatMessage(messages.pageInfo, {
              current: safePage + 1,
              total: totalPages,
            })}
          </span>
          <Button
            label={intl.formatMessage(messages.next)}
            onClick={handleNext}
            disabled={safePage >= totalPages - 1}
            skin={ButtonSkin}
          />
        </div>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryList);
