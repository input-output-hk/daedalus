import React from 'react';
import BigNumber from 'bignumber.js';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import globalMessages from '../../../i18n/global-messages';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepVotingPowerShare from '../_shared/DRepVotingPowerShare';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import { formatVotingPower } from '../_shared/drepVotingPower';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDirectoryTable.scss';

const messages = defineMessages({
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
  select: {
    id: 'governance.drepDirectory.card.select',
    defaultMessage: '!!!Delegate',
    description: 'Row-level CTA that hands the DRep ID to the delegation form',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry;
  isFavorite: boolean;
  onToggleFavorite: (drepId: string) => void;
  isCurrentDRep?: boolean;
  canDelegate?: boolean;
  totalDRepStake?: BigNumber | null;
  epochLength?: number | null;
  slotLength?: number | null;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepTableRow({
  entry,
  isFavorite,
  onToggleFavorite,
  isCurrentDRep = false,
  canDelegate = true,
  totalDRepStake = null,
  epochLength = null,
  slotLength = null,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  return (
    // Roles rather than table elements: the rows are windowed, which a real
    // tbody cannot be, and the roles keep the grid semantics either way.
    <div className={styles.row} role="row">
      <div className={`${styles.cell} ${styles.colFavorite}`} role="cell">
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
      </div>
      <div className={`${styles.cell} ${styles.colName}`} role="cell">
        {entry.verifiedName && (
          <span className={styles.name} title={entry.verifiedName}>
            {entry.verifiedName}
          </span>
        )}
        <DRepIdDisplay drepId={entry.drepId} variant="single" />
      </div>
      <div className={`${styles.cell} ${styles.colStatus}`} role="cell">
        <DRepStatusBadge
          status={entry.status}
          drepActivity={entry.drepActivity}
          epochLength={epochLength}
          slotLength={slotLength}
        />
      </div>
      <div className={`${styles.cell} ${styles.colVotingPower}`} role="cell">
        <span className={styles.votingPower}>
          {formatVotingPower(entry.votingPower)}
        </span>
        <DRepVotingPowerShare
          votingPower={entry.votingPower}
          totalDRepStake={totalDRepStake}
        />
      </div>
      <div className={`${styles.actionsCell} ${styles.colActions}`} role="cell">
        <Button
          className="flat"
          label={intl.formatMessage(globalMessages.viewDetails)}
          onClick={() => onViewDetails(entry.drepId)}
          skin={ButtonSkin}
        />
        {canDelegate && !isCurrentDRep && (
          <Button
            label={intl.formatMessage(messages.select)}
            onClick={() => onSelectForDelegation(entry.drepId)}
            skin={ButtonSkin}
          />
        )}
      </div>
    </div>
  );
}

export default injectIntl(DRepTableRow);
