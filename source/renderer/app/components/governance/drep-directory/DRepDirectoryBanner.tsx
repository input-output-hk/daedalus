import React from 'react';
import moment from 'moment';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import DRepIconButton from '../_shared/DRepIconButton';
// @ts-ignore inline svg module
import refreshIcon from '../../../assets/images/refresh.inline.svg';
import styles from './DRepDirectoryBanner.scss';

const messages = defineMessages({
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
    defaultMessage: '!!!Showing {n} DReps matching your filters.',
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
      {/* No page title here: the governance tab bar already names the page,
          the way the stake pools screen relies on its own tabs. */}
      <div className={styles.headerRow}>
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
        {/* An icon, not a page-sized CTA: refreshing is a small repeatable
            action, and a full green button shouted over the list it sits
            above. */}
        <DRepIconButton
          icon={refreshIcon}
          label={intl.formatMessage(messages.refresh)}
          onClick={onRefresh}
          disabled={isRefreshing}
        />
      </div>
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
