import BigNumber from 'bignumber.js';
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepDetailOnchainSection from './DRepDetailOnchainSection';
import DRepDetailAnchorSection from './DRepDetailAnchorSection';
import DRepDetailActions from './DRepDetailActions';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { GovernanceRefreshState } from '../../../stores/GovernanceStore';
import type { AppDRepDetail } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  backToDirectory: {
    id: 'governance.drepDirectory.backToDirectory',
    defaultMessage: '!!!Back to directory',
    description: 'Link from the DRep detail back to the directory',
  },
  loading: {
    id: 'governance.drepDetail.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Loading state on the DRep detail page',
  },
  notFound: {
    id: 'governance.drepDetail.notFound',
    defaultMessage: '!!!This DRep was not found in the on-chain data.',
    description: 'Inline error when the requested DRep is not in the index',
  },
});

interface Props {
  entry: AppDRepDetail | null;
  totalDRepStake?: BigNumber | null;
  epochLength?: number | null;
  slotLength?: number | null;
  refreshState: GovernanceRefreshState;
  onOpenExternalLink: (url: string) => void;
  network?: string;
  canDelegate?: boolean;
  isFavorite?: boolean;
  isCurrentDRep?: boolean;
  onSelectForDelegation: (drepId: string) => void;
  onToggleFavorite: (drepId: string) => void;
  onBackToDirectory: () => void;
  intl: intlShape.isRequired;
}

function DRepDetail({
  entry,
  totalDRepStake = null,
  epochLength = null,
  slotLength = null,
  refreshState,
  onOpenExternalLink,
  network,
  canDelegate = true,
  isFavorite = false,
  isCurrentDRep = false,
  onSelectForDelegation,
  onToggleFavorite,
  onBackToDirectory,
  intl,
}: Props) {
  const backLink = (
    <Link
      className={styles.backLink}
      label={intl.formatMessage(messages.backToDirectory)}
      hasIconAfter={false}
      onClick={onBackToDirectory}
      skin={LinkSkin}
    />
  );

  if (!entry) {
    // Deep links land here before the list query answers; only a settled
    // store (loaded or failed) may declare the DRep missing.
    if (
      refreshState === GovernanceRefreshState.Idle ||
      refreshState === GovernanceRefreshState.Loading ||
      refreshState === GovernanceRefreshState.Refreshing
    ) {
      return (
        <div className={styles.container}>
          {backLink}
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        </div>
      );
    }
    return (
      <div className={styles.container}>
        {backLink}
        <div className={styles.stateContainer}>
          <p className={styles.errorMessage}>
            {intl.formatMessage(messages.notFound)}
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className={styles.container}>
      {backLink}
      {/* The DRep itself titles this page. A generic "DRep detail" heading
          would only repeat what the back link and the tab bar already say. */}
      <div className={styles.identity}>
        {entry.verifiedName && (
          <h1 className={styles.drepName}>{entry.verifiedName}</h1>
        )}
        <DRepIdDisplay
          drepId={entry.drepId}
          variant="full"
          showCopiedConfirmation
        />
      </div>
      {/* Kept beside the identity rather than after the content: a DRep with
          a long profile would otherwise push the only way to act on it out of
          sight. */}
      {canDelegate && (
        <DRepDetailActions
          drepId={entry.drepId}
          isFavorite={isFavorite}
          isCurrentDRep={isCurrentDRep}
          onSelectForDelegation={onSelectForDelegation}
          onToggleFavorite={onToggleFavorite}
        />
      )}
      <DRepDetailOnchainSection
        entry={entry}
        totalDRepStake={totalDRepStake}
        network={network}
        onOpenExternalLink={onOpenExternalLink}
      />
      <DRepDetailAnchorSection
        network={network}
        anchor={entry.anchor}
        verifiedName={entry.verifiedName}
        metadata={entry.metadata}
        onOpenExternalLink={onOpenExternalLink}
      />
    </div>
  );
}

export default injectIntl(DRepDetail);
