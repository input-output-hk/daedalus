import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import globalMessages from '../../../i18n/global-messages';
import styles from './DRepDetail.scss';

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
});

interface Props {
  drepId: string;
  isFavorite: boolean;
  isCurrentDRep?: boolean;
  onSelectForDelegation: (drepId: string) => void;
  onToggleFavorite: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDetailActions({
  drepId,
  isFavorite,
  isCurrentDRep = false,
  onSelectForDelegation,
  onToggleFavorite,
  intl,
}: Props) {
  return (
    <div className={styles.actions}>
      {!isCurrentDRep && (
        <Button
          label={intl.formatMessage(globalMessages.delegate)}
          onClick={() => onSelectForDelegation(drepId)}
          skin={ButtonSkin}
        />
      )}
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
        onClick={() => onToggleFavorite(drepId)}
      >
        <span className={styles.favoriteIcon} aria-hidden="true">
          {isFavorite ? '★' : '☆'}
        </span>
        <span className={styles.favoriteLabel}>
          {intl.formatMessage(
            isFavorite ? messages.favoriteRemove : messages.favoriteAdd
          )}
        </span>
      </button>
    </div>
  );
}

export default injectIntl(DRepDetailActions);
