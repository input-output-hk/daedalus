import React from 'react';
import moment from 'moment';
import {
  FormattedMessage,
  defineMessages,
  injectIntl,
  intlShape,
} from 'react-intl';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import styles from './DRepErrorBanner.scss';

const messages = defineMessages({
  rankingUnavailable: {
    id: 'governance.drepDirectory.error.rankingUnavailable',
    defaultMessage:
      '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.',
    description: 'Non-blocking banner when the stake phase fails',
  },
  refreshFailed: {
    id: 'governance.drepDirectory.error.refresh',
    defaultMessage:
      "!!!Couldn't refresh DRep data. {Retry}. Showing last successful snapshot from {time}.",
    description:
      'Non-blocking banner when a refresh fails while a retained snapshot is on screen',
  },
});

export type DRepErrorBannerVariant = 'rankingUnavailable' | 'refreshFailed';

interface Props {
  variant: DRepErrorBannerVariant;
  retryLabel?: string;
  onRetry?: () => void;
  lastFetchedAt?: number | null;
  intl: intlShape.isRequired;
}

function DRepErrorBanner({
  variant,
  retryLabel = '',
  onRetry,
  lastFetchedAt = null,
  intl,
}: Props) {
  const messageByVariant = {
    rankingUnavailable: messages.rankingUnavailable,
    refreshFailed: messages.refreshFailed,
  };

  const body =
    variant === 'refreshFailed' ? (
      <FormattedMessage
        {...messageByVariant.refreshFailed}
        values={{
          Retry: (
            <Link
              className={styles.retryLink}
              label={retryLabel}
              hasIconAfter={false}
              onClick={onRetry}
              skin={LinkSkin}
            />
          ),
          time: lastFetchedAt ? moment(lastFetchedAt).fromNow() : '',
        }}
      />
    ) : (
      intl.formatMessage(messageByVariant[variant])
    );

  return (
    <div className={styles.banner} role="status" data-variant={variant}>
      <svg
        className={styles.icon}
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
      <span className={styles.message}>{body}</span>
    </div>
  );
}

export default injectIntl(DRepErrorBanner);
