import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepDirectoryList from './DRepDirectoryList';
import DRepDirectoryBanner from './DRepDirectoryBanner';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import {
  GovernanceRefreshState,
  AppDRepDirectoryEntry,
  GovernanceStoreError,
} from '../../../stores/GovernanceStore';
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
});

interface Props {
  drepList: AppDRepDirectoryEntry[];
  refreshState: GovernanceRefreshState;
  error: GovernanceStoreError | null;
  lastFetchedAt: number | null;
  onRefresh: () => void;
  intl: intlShape.isRequired;
}

function DRepDirectory({
  drepList,
  refreshState,
  error,
  lastFetchedAt,
  onRefresh,
  intl,
}: Props) {
  const hasRetainedData = drepList.length > 0;
  const showErrorBanner = error && hasRetainedData;

  const renderContent = () => {
    switch (true) {
      case refreshState === GovernanceRefreshState.Loading:
        return (
          <div className={styles.stateContainer}>
            <LoadingSpinner />
            <p>{intl.formatMessage(messages.loading)}</p>
          </div>
        );

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
            <DRepDirectoryList entries={drepList} />
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
      />
      {renderContent()}
    </div>
  );
}

export default injectIntl(DRepDirectory);
