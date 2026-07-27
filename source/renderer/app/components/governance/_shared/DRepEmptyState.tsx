import React from 'react';
import {
  FormattedMessage,
  defineMessages,
  injectIntl,
  intlShape,
} from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './DRepEmptyState.scss';

const messages = defineMessages({
  noSync: {
    id: 'governance.drepDirectory.empty.noSync',
    defaultMessage:
      '!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.',
    description: 'Directory fallback while the node has not reached the tip',
  },
  noResults: {
    id: 'governance.drepDirectory.empty.noResults',
    defaultMessage:
      '!!!No DReps match your filters. {ClearFilters} or {ShowAll}.',
    description: 'Empty state when search/filters match nothing',
  },
  clearFilters: {
    id: 'governance.drepDirectory.empty.noResults.clearFilters',
    defaultMessage: '!!!Clear filters',
    description: 'Action that resets search, filters and sort',
  },
  showAll: {
    id: 'governance.drepDirectory.cohortBanner.showAll',
    defaultMessage: '!!!Show all DReps',
    description: 'Toggle that switches from the default cohort to all DReps',
  },
  noFavoritesTitle: {
    id: 'governance.drepFavorites.empty.title',
    defaultMessage: '!!!No favorites yet',
    description: 'Title of the empty favorites view',
  },
  noFavoritesBody: {
    id: 'governance.drepFavorites.empty.body',
    defaultMessage:
      '!!!DReps you favorite from the directory appear here. Favorites are stored on this device only.',
    description:
      'Body of the empty favorites view; owns the per-device expectation',
  },
  backToDirectory: {
    id: 'governance.drepDirectory.backToDirectory',
    defaultMessage: '!!!Back to directory',
    description: 'CTA from the empty favorites view back to the directory',
  },
});

// noSync, noResults and noFavorites ship for now; the designed selfnode
// variant joins this union when its owning slice lands.
export type DRepEmptyStateVariant = 'noSync' | 'noResults' | 'noFavorites';

interface Props {
  variant: DRepEmptyStateVariant;
  onClearFilters?: () => void;
  onShowAll?: () => void;
  onBackToDirectory?: () => void;
  intl: intlShape.isRequired;
}

function DRepEmptyState({
  variant,
  onClearFilters,
  onShowAll,
  onBackToDirectory,
  intl,
}: Props) {
  if (variant === 'noResults') {
    return (
      <div className={styles.container} data-variant={variant}>
        <p className={styles.message}>
          <FormattedMessage
            {...messages.noResults}
            values={{
              ClearFilters: (
                <Link
                  className={styles.actionLink}
                  label={intl.formatMessage(messages.clearFilters)}
                  hasIconAfter={false}
                  onClick={onClearFilters}
                  skin={LinkSkin}
                />
              ),
              ShowAll: (
                <Link
                  className={styles.actionLink}
                  label={intl.formatMessage(messages.showAll)}
                  hasIconAfter={false}
                  onClick={onShowAll}
                  skin={LinkSkin}
                />
              ),
            }}
          />
        </p>
      </div>
    );
  }

  if (variant === 'noFavorites') {
    return (
      <div className={styles.container} data-variant={variant}>
        <p className={styles.title}>
          {intl.formatMessage(messages.noFavoritesTitle)}
        </p>
        <p className={styles.message}>
          {intl.formatMessage(messages.noFavoritesBody)}
        </p>
        <Link
          className={styles.actionLink}
          label={intl.formatMessage(messages.backToDirectory)}
          hasIconAfter={false}
          onClick={onBackToDirectory}
          skin={LinkSkin}
        />
      </div>
    );
  }

  return (
    <div className={styles.container} data-variant={variant}>
      <p className={styles.message}>{intl.formatMessage(messages.noSync)}</p>
    </div>
  );
}

export default injectIntl(DRepEmptyState);
