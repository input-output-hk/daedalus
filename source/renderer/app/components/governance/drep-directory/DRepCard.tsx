import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import globalMessages from '../../../i18n/global-messages';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepStatusBadge, { getDRepStanding } from '../_shared/DRepStatusBadge';
import DRepVotingPowerShare from '../_shared/DRepVotingPowerShare';
import { formatVotingPower } from '../_shared/drepVotingPower';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepCard.scss';

const messages = defineMessages({
  votingPowerLabel: {
    id: 'governance.drepDirectory.votingPower',
    defaultMessage: '!!!Voting power',
    description: 'Label for the voting power column in DRep directory',
  },
  select: {
    id: 'governance.drepDirectory.card.select',
    defaultMessage: '!!!Delegate',
    description: 'Row-level CTA that hands the DRep ID to the delegation form',
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
    defaultMessage: '!!!This DRep is not actively accepting delegation.',
    description: 'Inline caption under a stale favorited DRep',
  },
  currentDelegation: {
    id: 'governance.drepDirectory.card.currentDelegation',
    defaultMessage: '!!!Currently delegating',
    description: 'Badge shown on the card the user is currently delegated to',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry;
  isFavorite: boolean;
  onToggleFavorite: (drepId: string) => void;
  isStaleFavorite?: boolean;
  isCurrentDRep?: boolean;
  canDelegate?: boolean;
  totalDRepStake?: BigNumber | null;
  epochLength?: number | null;
  slotLength?: number | null;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepCard({
  entry,
  isFavorite,
  onToggleFavorite,
  isStaleFavorite = false,
  isCurrentDRep = false,
  canDelegate = true,
  totalDRepStake = null,
  epochLength = null,
  slotLength = null,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  const votingPowerTooltip =
    entry.votingPower === null
      ? intl.formatMessage(messages.votingPowerUnavailableTooltip)
      : undefined;

  const standing = getDRepStanding(entry.status, entry.drepActivity);
  const favoriteLabel = intl.formatMessage(
    isFavorite ? messages.favoriteRemove : messages.favoriteAdd
  );

  return (
    <div className={styles.card}>
      {/* Identity first: the star and the name are what a reader scans for,
          and the badges below qualify that identity rather than announce it. */}
      <div className={styles.identityRow}>
        <button
          type="button"
          className={styles.favoriteToggle}
          aria-pressed={isFavorite}
          aria-label={favoriteLabel}
          title={favoriteLabel}
          onClick={() => onToggleFavorite(entry.drepId)}
        >
          <span aria-hidden="true">{isFavorite ? '★' : '☆'}</span>
        </button>
        <div className={styles.identity}>
          {entry.verifiedName && (
            // Names reach 76 characters on mainnet and the card truncates at
            // a good deal less, so the full one has to stay reachable.
            <p className={styles.drepName} title={entry.verifiedName}>
              {entry.verifiedName}
            </p>
          )}
          <div className={styles.drepIdRow}>
            {/* Always the single form. The stacked dual-ID variant made the
                card taller than the grid row reserves for it, so a search
                turned the same component into a different, overflowing shape.
                Both ID forms are on the detail page, where there is room. */}
            <DRepIdDisplay drepId={entry.drepId} variant="single" />
          </div>
        </div>
      </div>
      {/* Badges here mark what is exceptional about a DRep, and a marker
          carried by almost every card tells a reader nothing. Active is the
          norm, so only its absence is worth saying. */}
      <div className={styles.badgeRow}>
        {standing !== 'active' && (
          <DRepStatusBadge
            status={entry.status}
            drepActivity={entry.drepActivity}
            epochLength={epochLength}
            slotLength={slotLength}
          />
        )}
        {isCurrentDRep && (
          <span className={styles.currentDelegationBadge}>
            {intl.formatMessage(messages.currentDelegation)}
          </span>
        )}
      </div>
      {isStaleFavorite && (
        <p className={styles.staleCaption}>
          {intl.formatMessage(messages.staleCaption)}
        </p>
      )}
      {/* The share sits with the figure it is a share of, so the two are read
          together rather than as unrelated facts at opposite ends of a card. */}
      <div className={styles.votingPowerRow}>
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
        <DRepVotingPowerShare
          votingPower={entry.votingPower}
          totalDRepStake={totalDRepStake}
        />
      </div>
      {/* One primary action per card. Selecting for delegation is what the
          directory exists to produce, so it carries the filled treatment;
          reading more about a DRep is a step along the way and takes the flat
          one. The table rows have always drawn the pair this way. */}
      <div className={styles.actionsRow}>
        {canDelegate && !isCurrentDRep && (
          <Button
            label={intl.formatMessage(messages.select)}
            onClick={() => onSelectForDelegation(entry.drepId)}
            skin={ButtonSkin}
          />
        )}
        <Button
          className="flat"
          label={intl.formatMessage(globalMessages.viewDetails)}
          onClick={() => onViewDetails(entry.drepId)}
          skin={ButtonSkin}
        />
      </div>
    </div>
  );
}

export default injectIntl(DRepCard);
