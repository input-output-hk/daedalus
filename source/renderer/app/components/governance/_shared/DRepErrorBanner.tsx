import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './DRepErrorBanner.scss';

const messages = defineMessages({
  rankingUnavailable: {
    id: 'governance.drepDirectory.error.rankingUnavailable',
    defaultMessage:
      '!!!Voting power data unavailable this refresh. Ranking-based filters disabled.',
    description: 'Non-blocking banner when the stake phase fails',
  },
});

// Only the rankingUnavailable variant ships for now; the designed
// refresh-failed variant joins this union when its owning slice lands.
export type DRepErrorBannerVariant = 'rankingUnavailable';

interface Props {
  variant: DRepErrorBannerVariant;
  intl: intlShape.isRequired;
}

function DRepErrorBanner({ variant, intl }: Props) {
  const messageByVariant = {
    rankingUnavailable: messages.rankingUnavailable,
  };

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
      <span className={styles.message}>
        {intl.formatMessage(messageByVariant[variant])}
      </span>
    </div>
  );
}

export default injectIntl(DRepErrorBanner);
