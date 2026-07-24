import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepCategoryBadge from '../_shared/DRepCategoryBadge';
import DRepDetailOnchainSection from './DRepDetailOnchainSection';
import DRepDetailAnchorSection from './DRepDetailAnchorSection';
import DRepDetailActions from './DRepDetailActions';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../stores/GovernanceStore';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDetail.title',
    defaultMessage: '!!!DRep detail',
    description: 'Title of the DRep detail page',
  },
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
    defaultMessage: '!!!This DRep was not found in the latest on-chain data.',
    description: 'Inline error when the requested DRep is not in the index',
  },
});

interface Props {
  entry: AppDRepDirectoryEntry | null;
  refreshState: GovernanceRefreshState;
  votingPowerState: VotingPowerEnrichState;
  onSelectForDelegation: (drepId: string) => void;
  onBackToDirectory: () => void;
  intl: intlShape.isRequired;
}

function DRepDetail({
  entry,
  refreshState,
  votingPowerState,
  onSelectForDelegation,
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
      <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
      <div className={styles.header}>
        <DRepIdDisplay drepId={entry.drepId} showCopiedConfirmation />
        <DRepCategoryBadge entry={entry} />
      </div>
      <DRepDetailOnchainSection
        entry={entry}
        votingPowerState={votingPowerState}
      />
      <DRepDetailAnchorSection anchor={entry.anchor} />
      <DRepDetailActions
        drepId={entry.drepId}
        onSelectForDelegation={onSelectForDelegation}
      />
    </div>
  );
}

export default injectIntl(DRepDetail);
