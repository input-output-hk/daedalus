import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import styles from './DRepEmptyState.scss';

const messages = defineMessages({
  noSync: {
    id: 'governance.drepDirectory.empty.noSync',
    defaultMessage:
      '!!!Your node is still syncing. DRep data becomes available once the node reaches the tip.',
    description: 'Directory fallback while the node has not reached the tip',
  },
});

// Only the noSync variant ships for now; the designed noResults and selfnode
// variants join this union when their owning slices land.
export type DRepEmptyStateVariant = 'noSync';

interface Props {
  variant: DRepEmptyStateVariant;
  intl: intlShape.isRequired;
}

function DRepEmptyState({ variant, intl }: Props) {
  const messageByVariant = {
    noSync: messages.noSync,
  };

  return (
    <div className={styles.container} data-variant={variant}>
      <p className={styles.message}>
        {intl.formatMessage(messageByVariant[variant])}
      </p>
    </div>
  );
}

export default injectIntl(DRepEmptyState);
