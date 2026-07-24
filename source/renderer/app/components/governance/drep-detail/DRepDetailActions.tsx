import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  select: {
    id: 'governance.drepDirectory.card.select',
    defaultMessage: '!!!Select for delegation',
    description: 'CTA that hands the DRep ID to the delegation form',
  },
});

interface Props {
  drepId: string;
  onSelectForDelegation: (drepId: string) => void;
  intl: intlShape.isRequired;
}

function DRepDetailActions({ drepId, onSelectForDelegation, intl }: Props) {
  return (
    <div className={styles.actions}>
      <Button
        label={intl.formatMessage(messages.select)}
        onClick={() => onSelectForDelegation(drepId)}
        skin={ButtonSkin}
      />
    </div>
  );
}

export default injectIntl(DRepDetailActions);
