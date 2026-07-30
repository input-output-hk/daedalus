import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepCategoryBadge from '../_shared/DRepCategoryBadge';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
import type {
  AppDRepDirectoryEntry,
  DRepCohortContext,
} from '../../../stores/GovernanceStore';
import { VotingPowerEnrichState } from '../../../stores/GovernanceStore';
import styles from './DRepCard.scss';

const messages = defineMessages({
  votingPowerLabel: {
    id: 'governance.drepDirectory.votingPower',
    defaultMessage: '!!!Voting power',
    description: 'Label for the voting power column in DRep directory',
  },
  select: {
    id: 'governance.drepDirectory.card.select',
    defaultMessage: '!!!Select for delegation',
    description: 'Row-level CTA that hands the DRep ID to the delegation form',
  },
  viewDetails: {
    id: 'governance.drepDirectory.card.viewDetails',
    defaultMessage: '!!!View details',
    description: 'Card CTA that opens the DRep detail view',
  },
  votingPowerLoadingTooltip: {
    id: 'governance.drepDirectory.votingPower.loadingTooltip',
    defaultMessage: '!!!Loading voting power…',
    description: 'Tooltip on the voting-power placeholder during enrichment',
  },
  votingPowerUnavailableTooltip: {
    id: 'governance.drepDirectory.votingPower.unavailableTooltip',
    defaultMessage: '!!!Stake distribution unavailable this refresh.',
    description: 'Tooltip on the voting-power placeholder when stake failed',
  },
  favoriteAdd: {
    id: 'governance.drepDirectory.card.favorite.add',
    defaultMessage: '!!!Add to favorites',
    description: 'Accessible label of the favorite toggle when not favorited',
  },
  favoriteRemove: {
    id: 'governance.drepDirectory.card.favorite.remove',
    defaultMessage: '!!!Remove from favorites',
    description: 'Accessible label of the favorite toggle when favorited',
  },
  staleCaption: {
    id: 'governance.drepFavorites.staleCaption',
    defaultMessage: '!!!This DRep is no longer in the default cohort.',
    description: 'Inline caption under a stale favorited DRep',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry;
  cohort: DRepCohortContext;
  isFavorite: boolean;
  onToggleFavorite: (drepId: string) => void;
  isStaleFavorite?: boolean;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  votingPowerState: VotingPowerEnrichState;
  intl: intlShape.isRequired;
}

function formatVotingPower(value: BigNumber | null): string {
  if (!value) return '—';
  // Human-rounded ADA with ₳ glyph
  const ada = value.div(1_000_000);
  if (ada.isGreaterThanOrEqualTo(1_000_000)) {
    return `₳ ${ada.div(1_000_000).toFormat(1)}M`;
  }
  if (ada.isGreaterThanOrEqualTo(1_000)) {
    return `₳ ${ada.div(1_000).toFormat(1)}K`;
  }
  return `₳ ${ada.toFormat(0)}`;
}

function DRepCard({
  entry,
  cohort,
  isFavorite,
  onToggleFavorite,
  isStaleFavorite = false,
  onSelectForDelegation,
  onViewDetails,
  votingPowerState,
  intl,
}: Props) {
  // Native title/aria-label keep the placeholder accessible without a
  // PopOver dependency; loading vs unavailable follows the enrich state.
  const votingPowerTooltip =
    entry.votingPower === null
      ? intl.formatMessage(
          votingPowerState === VotingPowerEnrichState.Loading
            ? messages.votingPowerLoadingTooltip
            : messages.votingPowerUnavailableTooltip
        )
      : undefined;

  return (
    <div className={styles.card}>
      <div className={styles.topRow}>
        <button
          type="button"
          className={styles.favoriteToggle}
          aria-pressed={isFavorite}
          aria-label={intl.formatMessage(
            isFavorite ? messages.favoriteRemove : messages.favoriteAdd
          )}
          title={intl.formatMessage(
            isFavorite ? messages.favoriteRemove : messages.favoriteAdd
          )}
          onClick={() => onToggleFavorite(entry.drepId)}
        >
          <span aria-hidden="true">{isFavorite ? '★' : '☆'}</span>
        </button>
        <DRepStatusBadge status={entry.status} />
        <DRepCategoryBadge entry={entry} cohort={cohort} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
      {isStaleFavorite && (
        <p className={styles.staleCaption}>
          {intl.formatMessage(messages.staleCaption)}
        </p>
      )}
      <div className={styles.bottomRow}>
        <span className={styles.votingPowerLabel}>
          {intl.formatMessage(messages.votingPowerLabel)}:
        </span>
        <span
          className={styles.votingPowerValue}
          title={votingPowerTooltip}
          aria-label={votingPowerTooltip}
        >
          {formatVotingPower(entry.votingPower)}
        </span>
        <DRepSourceLabel className={styles.sourceLabel} source="on-chain" />
      </div>
      <div className={styles.actionsRow}>
        <Button
          label={intl.formatMessage(messages.viewDetails)}
          onClick={() => onViewDetails(entry.drepId)}
          skin={ButtonSkin}
        />
        <Button
          label={intl.formatMessage(messages.select)}
          onClick={() => onSelectForDelegation(entry.drepId)}
          skin={ButtonSkin}
        />
      </div>
    </div>
  );
}

export default injectIntl(DRepCard);
