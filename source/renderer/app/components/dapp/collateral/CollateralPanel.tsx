import React from 'react';
import { injectIntl } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type {
  CollateralPreference,
  CollateralState,
} from '../../../../../common/types/collateral.types';
import type { Intl } from '../../../types/i18nTypes';
import messages from './CollateralPanel.messages';
import styles from './CollateralPanel.scss';

export type CollateralPanelProps = Readonly<{
  preference?: CollateralPreference;
  corrupt: boolean;
  busy: boolean;
  failed: boolean;
  onPrepare: () => void;
  onCancelPreparation: () => void;
  onClear: () => void;
  onRepair: () => void;
}>;

type Props = CollateralPanelProps & { intl: Intl };

const stateMessage = (state: CollateralState) =>
  (({
    checking: messages.checking,
    ready: messages.ready,
    'not-ready': messages.notReady,
    preparing: messages.preparing,
    'in-use': messages.inUse,
    'will-be-spent': messages.willBeSpent,
    charged: messages.charged,
    stale: messages.stale,
  } as const)[state]);

export function CollateralPanel({
  intl,
  preference,
  corrupt,
  busy,
  failed,
  onPrepare,
  onCancelPreparation,
  onClear,
  onRepair,
}: Props) {
  const state = preference?.state ?? 'checking';
  const canPrepare =
    state === 'not-ready' || state === 'charged' || state === 'stale';
  const canClear = !!preference?.preferredInputs.length;

  return (
    <section className={styles.component} aria-labelledby="collateral-title">
      <h2 id="collateral-title">{intl.formatMessage(messages.title)}</h2>
      <p className={styles.convention}>
        {intl.formatMessage(messages.convention)}
      </p>
      <p className={styles.status} role="status">
        {intl.formatMessage(stateMessage(state))}
      </p>
      {failed && (
        <p className={styles.error} role="alert">
          {intl.formatMessage(messages.failed)}
        </p>
      )}
      <div className={styles.actions}>
        {corrupt ? (
          <Button
            skin={ButtonSkin}
            label={intl.formatMessage(messages.repair)}
            disabled={busy}
            onClick={onRepair}
          />
        ) : (
          <>
            {canPrepare && (
              <Button
                skin={ButtonSkin}
                label={intl.formatMessage(messages.prepare)}
                disabled={busy}
                onClick={onPrepare}
              />
            )}
            {state === 'preparing' && (
              <Button
                skin={ButtonSkin}
                label={intl.formatMessage(messages.cancelPreparation)}
                disabled={busy}
                onClick={onCancelPreparation}
              />
            )}
            {canClear && (
              <Button
                skin={ButtonSkin}
                label={intl.formatMessage(messages.clear)}
                disabled={busy}
                onClick={onClear}
              />
            )}
          </>
        )}
      </div>
    </section>
  );
}

export default injectIntl(CollateralPanel);
