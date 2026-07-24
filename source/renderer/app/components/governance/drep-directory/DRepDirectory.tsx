import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepDirectoryList from './DRepDirectoryList';
import DRepDirectoryBanner from './DRepDirectoryBanner';
import DRepEmptyState from '../_shared/DRepEmptyState';
import DRepErrorBanner from '../_shared/DRepErrorBanner';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import {
  GovernanceRefreshState,
  VotingPowerEnrichState,
  AppDRepDirectoryEntry,
  GovernanceStoreError,
} from '../../../stores/GovernanceStore';
import { GovernanceQueryErrorType } from '../../../../../common/types/governance.types';
import styles from './DRepDirectory.scss';

const messages = defineMessages({
  title: {
    id: 'governance.drepDirectory.title',
    defaultMessage: '!!!DRep Directory',
    description: 'Title of the DRep directory page',
  },
  loading: {
    id: 'governance.drepDirectory.loading',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Loading state message',
  },
  empty: {
    id: 'governance.drepDirectory.empty',
    defaultMessage: '!!!No DReps found on-chain.',
    description: 'Empty directory state',
  },
  error: {
    id: 'governance.drepDirectory.error',
    defaultMessage: '!!!Could not load DRep data.',
    description: 'Error state message',
  },
  retry: {
    id: 'governance.drepDirectory.retry',
    defaultMessage: '!!!Retry',
    description: 'Retry button label',
  },
  refreshing: {
    id: 'governance.drepDirectory.refreshing',
    defaultMessage: '!!!Refreshing…',
    description: 'Refreshing state badge label',
  },
  syncing: {
    id: 'governance.drepDirectory.syncing',
    defaultMessage:
      '!!!Your node is still syncing ({progress}%). The DRep list may be incomplete until sync completes.',
    description: 'Persistent soft-warning banner while the node is syncing',
  },
});

interface Props {
  drepList: AppDRepDirectoryEntry[];
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  isNodeInSync: boolean;
  syncProgress: number | null;
  votingPowerState: VotingPowerEnrichState;
  isCohortActive: boolean;
  onRefresh: () => void;
  onReshuffle: () => void;
  onSelectForDelegation: (drepId: string) => void;
  onViewDetails: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectory({
  drepList,
  refreshState,
  error,
  lastFetchedAt,
  isNodeInSync,
  syncProgress,
  votingPowerState,
  isCohortActive,
  onRefresh,
  onReshuffle,
  onSelectForDelegation,
  onViewDetails,
  intl,
}: Props) {
  const hasRetainedData = drepList.length > 0;
  const showErrorBanner = error && hasRetainedData;

  // While syncing, an empty or unavailable directory is expected — fall back
  // to the noSync empty state instead of a bare error or "No DReps found".
  const showNoSyncFallback =
    !isNodeInSync &&
    !hasRetainedData &&
    (refreshState === GovernanceRefreshState.Loaded ||
      (refreshState === GovernanceRefreshState.Failed &&
        error?.type !== GovernanceQueryErrorType.SelfnodeCliUnsupported));

  const renderContent = () => {
    switch (true) {
      case refreshState === GovernanceRefreshState.Loading:
        return (
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        );

      case showNoSyncFallback:
        return <DRepEmptyState variant="noSync" />;

      case refreshState === GovernanceRefreshState.Failed:
        return (
          <div className={styles.stateContainer}>
            <p className={styles.errorMessage}>
              {intl.formatMessage(messages.error)}
            </p>
            {error && <p className={styles.errorDetails}>{error.message}</p>}
            {error?.details && (
              <p className={styles.errorDetails}>{error.details}</p>
            )}
            <Button
              label={intl.formatMessage(messages.retry)}
              onClick={onRefresh}
              skin={ButtonSkin}
            />
          </div>
        );

      case drepList.length === 0 &&
        refreshState === GovernanceRefreshState.Loaded:
        return (
          <div className={styles.stateContainer}>
            <p>{intl.formatMessage(messages.empty)}</p>
            <Button
              label={intl.formatMessage(messages.retry)}
              onClick={onRefresh}
              skin={ButtonSkin}
            />
          </div>
        );

      default:
        return (
          <>
            {showErrorBanner && error && (
              <div className={styles.errorBanner}>
                <div>
                  <p className={styles.errorMessage}>
                    {intl.formatMessage(messages.error)}
                  </p>
                  <p className={styles.errorDetails}>{error.message}</p>
                  {error.details && (
                    <p className={styles.errorDetails}>{error.details}</p>
                  )}
                </div>
                <Button
                  label={intl.formatMessage(messages.retry)}
                  onClick={onRefresh}
                  skin={ButtonSkin}
                />
              </div>
            )}
            {refreshState === GovernanceRefreshState.Refreshing && (
              <div className={styles.refreshingBadge}>
                <LoadingSpinner />
                {intl.formatMessage(messages.refreshing)}
              </div>
            )}
            {votingPowerState === VotingPowerEnrichState.Failed && (
              <DRepErrorBanner variant="rankingUnavailable" />
            )}
            <DRepDirectoryList
              entries={drepList}
              onSelectForDelegation={onSelectForDelegation}
              onViewDetails={onViewDetails}
              votingPowerState={votingPowerState}
            />
          </>
        );
    }
  };

  return (
    <div className={styles.container}>
      <DRepDirectoryBanner
        lastFetchedAt={lastFetchedAt}
        onRefresh={onRefresh}
        isRefreshing={refreshState === GovernanceRefreshState.Refreshing}
        isCohortActive={isCohortActive}
        onReshuffle={onReshuffle}
      />
      {!isNodeInSync && (
        <div className={styles.syncingBanner} role="status">
          <svg
            className={styles.syncingIcon}
            aria-hidden="true"
            width="16"
            height="16"
            viewBox="0 0 16 16"
          >
            <path
              d="M8 1.5 15 14H1L8 1.5z"
              fill="none"
              stroke="currentColor"
              strokeWidth="1.5"
              strokeLinejoin="round"
            />
            <path d="M8 6v4" stroke="currentColor" strokeWidth="1.5" />
            <circle cx="8" cy="12" r="0.9" fill="currentColor" />
          </svg>
          <span>
            {intl.formatMessage(messages.syncing, {
              progress: Math.floor(syncProgress ?? 0),
            })}
          </span>
        </div>
      )}
      {renderContent()}
    </div>
  );
}

export default injectIntl(DRepDirectory);
