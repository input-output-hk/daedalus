import React from 'react';
import moment from 'moment';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './DRepDirectoryBanner.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDirectory.title',
    defaultMessage: '!!!DRep Directory',
    description: 'Title banner for DRep directory',
  },
  refresh: {
    id: 'governance.drepDirectory.refresh',
    defaultMessage: '!!!Refresh',
    description: 'Refresh button label',
  },
  lastUpdated: {
    id: 'governance.drepDirectory.lastUpdated',
    defaultMessage: '!!!Last updated {time}',
    description: 'Last updated timestamp label',
  },
  cohortBanner: {
    id: 'governance.drepDirectory.cohortBanner',
    defaultMessage:
      '!!!Default view shows up to 200 eligible DReps in randomized order, excluding the 35 largest by voting power.',
    description: 'Primary line explaining the randomized default cohort',
  },
  reshuffle: {
    id: 'governance.drepDirectory.cohortBanner.reshuffle',
    defaultMessage: '!!!Reshuffle order',
    description: 'Control that reseeds the randomized cohort order',
  },
  source: {
    id: 'governance.drepDirectory.cohortBanner.source',
    defaultMessage:
      '!!!Cohort sizing follows the Beyond MVG (BMVG) Simplified one-click-delegation analysis.',
    description: 'Secondary line crediting the BMVG cohort-sizing analysis',
  },
  filtered: {
    id: 'governance.drepDirectory.cohortBanner.filtered',
    defaultMessage:
      '!!!Showing {n} DReps matching your filters. Default randomized order does not apply.',
    description: 'Banner line replacing the cohort claim while filtered',
  },
});

interface Props {
  lastFetchedAt: number | null;
  onRefresh: () => void;
  isRefreshing: boolean;
  isCohortActive: boolean;
  onReshuffle: () => void;
  // Story-only escape hatch; production call sites always keep the default true.
  showSource?: boolean;
  // Both default to the pure-default-view state so existing call sites and
  // stories keep compiling unchanged.
  isFilteredView?: boolean;
  displayedCount?: number;
  intl: intlShape.isRequired;
}

function DRepDirectoryBanner({
  lastFetchedAt,
  onRefresh,
  isRefreshing,
  isCohortActive,
  onReshuffle,
  showSource = true,
  isFilteredView = false,
  displayedCount = 0,
  intl,
}: Props) {
  const timeAgo = lastFetchedAt ? moment(lastFetchedAt).fromNow() : null;

  return (
    <div className={styles.banner}>
      <div className={styles.headerRow}>
        <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
        <Button
          label={intl.formatMessage(messages.refresh)}
          onClick={onRefresh}
          disabled={isRefreshing}
          skin={ButtonSkin}
        />
      </div>
      {lastFetchedAt && timeAgo !== null && (
        <p className={styles.lastUpdated}>
          {intl.formatMessage(messages.lastUpdated, {
            time: timeAgo,
          })}
        </p>
      )}
      {isCohortActive && !isFilteredView && (
        <div className={styles.cohortLine}>
          <span>{intl.formatMessage(messages.cohortBanner)}</span>
          <Link
            className={styles.reshuffleLink}
            label={intl.formatMessage(messages.reshuffle)}
            hasIconAfter={false}
            onClick={onReshuffle}
            skin={LinkSkin}
          />
        </div>
      )}
      {isCohortActive && !isFilteredView && showSource && (
        <p className={styles.sourceLine}>
          {intl.formatMessage(messages.source)}
        </p>
      )}
      {isFilteredView && (
        <p className={styles.filteredLine}>
          {intl.formatMessage(messages.filtered, { n: displayedCount })}
        </p>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryBanner);
