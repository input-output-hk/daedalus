import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import type {
  ChainDamage,
  ChainDamageKind,
} from '../../../../../common/types/watchdog.types';
import messages from './ChainDamage.messages';
import styles from './ChainDamageView.scss';

const SYMPTOM_BY_KIND: Record<ChainDamageKind, string> = {
  node_rejects_canonical_block: 'symptomNodeRejectsCanonicalBlock',
  ledger_point_missing: 'symptomLedgerPointMissing',
  wallet_chain_discontinuity: 'symptomWalletChainDiscontinuity',
};

type Props = {
  damage: ChainDamage;
  // Which view opens first; only the stories start on the confirmation
  initialView?: 'choice' | 'confirm';
  onRebuild: () => void;
  onDismiss: () => void;
};

type State = {
  view: 'choice' | 'confirm';
  isStarted: boolean;
};

export default class ChainDamageView extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state: State = {
    view: this.props.initialView ?? 'choice',
    isStarted: false,
  };

  showConfirmation = () => {
    this.setState({ view: 'confirm' });
  };

  cancelConfirmation = () => {
    this.setState({ view: 'choice' });
  };

  // The only caller of onRebuild, so the confirmation always precedes the
  // rebuild. Once sent, the button stays disabled until the Mithril progress
  // view replaces this one, so a second click cannot send it again.
  handleRebuild = () => {
    if (this.state.isStarted) return;
    this.setState({ isStarted: true });
    this.props.onRebuild();
  };

  renderChoiceView() {
    const { damage, onDismiss } = this.props;
    const { intl } = this.context;
    const symptom = messages[SYMPTOM_BY_KIND[damage.kind]];

    return (
      <div className={styles.root}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.title)}</h1>
          {symptom && <p>{intl.formatMessage(symptom)}</p>}
          <p>{intl.formatMessage(messages.body)}</p>
        </div>
        <div className={styles.detail}>
          {damage.slot == null
            ? intl.formatMessage(messages.detailNoSlot, { kind: damage.kind })
            : intl.formatMessage(messages.detail, {
                kind: damage.kind,
                slot: damage.slot,
              })}
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
            label={intl.formatMessage(messages.rebuild)}
            onClick={this.showConfirmation}
          />
        </div>
      </div>
    );
  }

  renderConfirmView() {
    const { intl } = this.context;
    const { isStarted } = this.state;

    return (
      <div className={styles.root}>
        <div className={styles.header}>
          <h1>{intl.formatMessage(messages.confirmTitle)}</h1>
          <p>{intl.formatMessage(messages.confirmBody)}</p>
        </div>
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.confirmBack)}
            disabled={isStarted}
            onClick={this.cancelConfirmation}
          />
          <Button
            className={styles.primaryAction}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.confirmRebuild)}
            disabled={isStarted}
            onClick={this.handleRebuild}
          />
        </div>
      </div>
    );
  }

  render() {
    return this.state.view === 'confirm'
      ? this.renderConfirmView()
      : this.renderChoiceView();
  }
}
