import React from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type { Intl } from '../../../types/i18nTypes';
import messages from './MithrilBootstrap.messages';
import { MITHRIL_DECISION_HEADING_ID } from './accessibilityIds';
import styles from './MithrilDecisionView.scss';

interface Props {
  onAccept(): void;
  onDecline(): void;
}

interface Context {
  intl: Intl;
}

function MithrilDecisionView({ onAccept, onDecline }: Props, { intl }: Context) {
  return (
    <div className={styles.root}>
      <div className={styles.header}>
        <h1 id={MITHRIL_DECISION_HEADING_ID}>
          {intl.formatMessage(messages.title)}
        </h1>
        <p>{intl.formatMessage(messages.description)}</p>
      </div>

      <div className={styles.actions}>
        <Button
          className={styles.secondaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.decline)}
          onClick={onDecline}
        />
        <Button
          className={styles.primaryAction}
          skin={ButtonSkin}
          label={intl.formatMessage(messages.accept)}
          onClick={onAccept}
        />
      </div>
    </div>
  );
}

MithrilDecisionView.contextTypes = {
  intl: intlShape.isRequired,
};

export default MithrilDecisionView;
