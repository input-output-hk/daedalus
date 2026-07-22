import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepSourceLabel from '../_shared/DRepSourceLabel';
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
    defaultMessage: '!!!Select for delegation',
    description: 'Row-level CTA that hands the DRep ID to the delegation form',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry;
  onSelectForDelegation: (drepId: string) => void;
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

function DRepCard({ entry, onSelectForDelegation, intl }: Props) {
  return (
    <div className={styles.card}>
      <div className={styles.topRow}>
        <DRepStatusBadge status={entry.status} />
        <DRepIdDisplay drepId={entry.drepId} />
      </div>
      <div className={styles.bottomRow}>
        <span className={styles.votingPowerLabel}>
          {intl.formatMessage(messages.votingPowerLabel)}:
        </span>
        <span className={styles.votingPowerValue}>
          {formatVotingPower(entry.votingPower)}
        </span>
        <DRepSourceLabel className={styles.sourceLabel} source="on-chain" />
      </div>
      <div className={styles.actionsRow}>
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
