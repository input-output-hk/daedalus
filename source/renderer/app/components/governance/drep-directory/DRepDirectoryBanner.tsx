import React from 'react';
import moment from 'moment';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import LoadingSpinner from '../../widgets/LoadingSpinner';
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
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Accessible label of the refresh-in-flight badge',
  },
  filtered: {
    id: 'governance.drepDirectory.cohortBanner.filtered',
    defaultMessage:
      '!!!Showing {n} DReps matching your filters. Default randomized order does not apply.',
    description: 'Banner line replacing the cohort claim while filtered',
  },
  favorites: {
    id: 'governance.drepFavorites.banner',
    defaultMessage:
      "!!!{n} DReps you've favorited. Favorites are stored on this device only.",
    description:
      'Banner line of the favorites view, replacing the cohort claim',
  },
});

interface Props {
  lastFetchedAt: number | null;
  onRefresh: () => void;
  isRefreshing: boolean;
  isFilteredView?: boolean;
  displayedCount?: number;
  isFavoritesView?: boolean;
  favoritesCount?: number;
  intl: intlShape.isRequired;
}

function DRepDirectoryBanner({
  lastFetchedAt,
  onRefresh,
  isRefreshing,
  isFilteredView = false,
  displayedCount = 0,
  isFavoritesView = false,
  favoritesCount = 0,
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
        <div className={styles.lastUpdated}>
          {intl.formatMessage(messages.lastUpdated, {
            time: timeAgo,
          })}
          {isRefreshing && (
            <span className={styles.refreshingBadge} role="status">
              <LoadingSpinner />
              {intl.formatMessage(messages.refreshing)}
            </span>
          )}
        </div>
      )}
      {isFilteredView && !isFavoritesView && (
        <p className={styles.filteredLine}>
          {intl.formatMessage(messages.filtered, { n: displayedCount })}
        </p>
      )}
      {isFavoritesView && (
        <p className={styles.favoritesLine}>
          {intl.formatMessage(messages.favorites, { n: favoritesCount })}
        </p>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectoryBanner);
