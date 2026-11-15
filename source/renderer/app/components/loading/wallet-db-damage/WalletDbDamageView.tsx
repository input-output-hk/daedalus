import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type {
  WalletDbRepairOutcome,
  WalletDbRepairResult,
  WalletDbRepairView,
} from '../../../../../common/types/watchdog.types';
import type { ReactIntlMessage } from '../../../types/i18nTypes';
import messages from './WalletDbDamage.messages';
import styles from '../chain-damage/ChainDamageView.scss';

// Why a file was not repaired, in plain words. An outcome this version does
// not know gets reasonOther; the outcome itself is in watchdog.log.
const REASON_BY_OUTCOME: Partial<
  Record<WalletDbRepairOutcome, ReactIntlMessage>
> = {
  refused: messages.reasonRefused,
  backup_failed: messages.reasonBackupFailed,
  repair_failed: messages.reasonRepairFailed,
};

type Props = {
  view: WalletDbRepairView;
  // Wallet data files that cannot be opened, for the offer
  damagedCount: number;
  // What the repair did to each file, once it has run
  results: Array<WalletDbRepairResult>;
  backupDir: string | null;
  onRepair: () => void;
  // Not now on the offer, Continue after the results
  onDismiss: () => void;
};

export default class WalletDbDamageView extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  renderOffer() {
    const { damagedCount, onRepair, onDismiss } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.root}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.title)}</h1>
          <p>{intl.formatMessage(messages.body, { count: damagedCount })}</p>
          <p>{intl.formatMessage(messages.offer)}</p>
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.notNow)}
            onClick={onDismiss}
          />
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.repair)}
            onClick={onRepair}
          />
        </div>
      </div>
    );
  }

  renderRepairing() {
    const { intl } = this.context;
    return (
      <div className={styles.root}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.repairingTitle)}</h1>
          <p>{intl.formatMessage(messages.repairingBody)}</p>
        </div>
      </div>
    );
  }

  renderDone() {
    const { results, backupDir, onDismiss } = this.props;
    const { intl } = this.context;
    const repaired = results.filter((r) => r.outcome === 'repaired').length;
    const notRepaired = results.filter((r) => r.outcome !== 'repaired');
    return (
      <div className={styles.root}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.doneTitle)}</h1>
          {results.length === 0 && (
            <p>{intl.formatMessage(messages.nothingToRepair)}</p>
          )}
          {repaired > 0 && (
            <p>{intl.formatMessage(messages.repaired, { count: repaired })}</p>
          )}
          {notRepaired.length > 0 && (
            <p>
              {intl.formatMessage(messages.notRepaired, {
                count: notRepaired.length,
              })}
            </p>
          )}
        </div>
        {(backupDir != null || notRepaired.length > 0) && (
          <div className={styles.detail}>
            {backupDir != null && (
              <p>
                {intl.formatMessage(messages.backupLocation, {
                  folder: backupDir,
                })}
              </p>
            )}
            {notRepaired.map((r) => (
              <p key={r.file}>
                {intl.formatMessage(
                  REASON_BY_OUTCOME[r.outcome] ?? messages.reasonOther,
                  { file: r.file }
                )}
              </p>
            ))}
          </div>
        )}
        <div className={styles.actions}>
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.continue)}
            onClick={onDismiss}
          />
        </div>
      </div>
    );
  }

  render() {
    switch (this.props.view) {
      case 'repairing':
        return this.renderRepairing();
      case 'done':
        return this.renderDone();
      default:
        return this.renderOffer();
    }
  }
}
