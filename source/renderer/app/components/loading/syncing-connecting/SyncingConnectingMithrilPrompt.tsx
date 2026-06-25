import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';

import { logger } from '../../../utils/logging';
import styles from './SyncingConnectingMithrilPrompt.scss';

const messages = defineMessages({
  promptBody: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptBody',
    defaultMessage: '!!!Your node is about {epochs} epochs behind.',
    description:
      'Epochs-behind body line (line 1) for the proactive Mithril fork prompt on the syncing screen',
  },
  promptBodyUnknown: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptBodyUnknown',
    defaultMessage: '!!!Your node is behind the blockchain tip.',
    description:
      'Body line (line 1) shown when the epochs-behind figure is unavailable for the proactive Mithril fork prompt',
  },
  promptBodyBenefit: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptBodyBenefit',
    defaultMessage:
      '!!!Mithril can catch you up faster than the standard sync.',
    description:
      'Benefit-vs-standard-sync body line (line 2) for the proactive Mithril fork prompt',
  },
  promptHandoffNote: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote',
    defaultMessage:
      '!!!If skipped, you can still start the Mithril sync from the Diagnostics screen.',
    description:
      'Handoff note telling the user they can start Mithril later from Diagnostics if they skip the proactive prompt',
  },
  promptMithrilButton: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptMithrilButton',
    defaultMessage: '!!!Mithril Sync (fast)',
    description:
      'Primary choice-view button label that opens the concise inline confirm step for the proactive Mithril fork prompt',
  },
  promptStandardButton: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptStandardButton',
    defaultMessage: '!!!Standard Sync (slow)',
    description:
      'Secondary choice-view button label that dismisses the proactive Mithril fork prompt for this session',
  },
  promptConfirmBody: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptConfirmBody',
    defaultMessage:
      '!!!Mithril will stop your Cardano node, restore verified chain data, and restart it — so you catch up faster.',
    description:
      'Concise confirm-view body for the proactive Mithril fork prompt on the syncing screen',
  },
  promptConfirmStart: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptConfirmStart',
    defaultMessage: '!!!Start now',
    description:
      'Confirm-view primary button label that starts the Mithril sync for the proactive Mithril fork prompt',
  },
  promptConfirmCancel: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptConfirmCancel',
    defaultMessage: '!!!Cancel',
    description:
      'Confirm-view secondary button label that returns to the choice view for the proactive Mithril fork prompt',
  },
});

type Props = {
  behindByEpochs?: number;
  onStart: () => Promise<void>;
  onDismiss: () => void;
};

type State = {
  view: 'choice' | 'confirm';
  isStarting: boolean;
  startError: string | null;
};

export default class SyncingConnectingMithrilPrompt extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state: State = {
    view: 'choice',
    isStarting: false,
    startError: null,
  };
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  showConfirmation = () => {
    this.setState({ view: 'confirm', startError: null });
  };

  cancelConfirmation = () => {
    this.setState({ view: 'choice', startError: null });
  };

  // The choice-view fast button NEVER starts Mithril sync — it only reveals the
  // confirm view; `handleStart` (the confirm-view "Start now") is the single
  // call site of `onStart` (the store's `startPartialSync`), so confirmation
  // always precedes start.
  handleStart = async () => {
    this.setState({ isStarting: true, startError: null });
    try {
      await this.props.onStart();
      // On success the store flips status to a working/overlay status
      // (`stopping-node`); the syncing screen yields to the partial-sync overlay
      // and this prompt naturally unmounts. The container gate stays true; the
      // overlay simply routes in. "Start now" is disabled via `isStarting` so a
      // brief lingering mount cannot trigger a second start.
    } catch (error) {
      logger.warn(
        'SyncingConnectingMithrilPrompt: Mithril sync start rejected after confirmation',
        { error }
      );
      if (!this._isMounted) {
        return;
      }

      this.setState({
        isStarting: false,
        startError:
          error instanceof Error
            ? error.message
            : 'Unable to start Mithril sync.',
      });
    }
  };

  renderChoiceView() {
    const { behindByEpochs, onDismiss } = this.props;
    const { intl } = this.context;

    const hasBehindFigure =
      typeof behindByEpochs === 'number' && Number.isFinite(behindByEpochs);

    return (
      <div className={styles.component}>
        <p className={styles.body}>
          {hasBehindFigure
            ? intl.formatMessage(messages.promptBody, {
                epochs: behindByEpochs,
              })
            : intl.formatMessage(messages.promptBodyUnknown)}
        </p>
        <p className={styles.benefit}>
          {intl.formatMessage(messages.promptBodyBenefit)}
        </p>
        <p className={styles.handoffNote}>
          {intl.formatMessage(messages.promptHandoffNote)}
        </p>
        <div className={styles.actions}>
          <Button
            className={classNames([styles.actionButton])}
            label={intl.formatMessage(messages.promptStandardButton)}
            onClick={onDismiss}
            skin={ButtonSkin}
          />
          <Button
            className={classNames([
              'primary',
              styles.actionButton,
              styles.primaryButton,
            ])}
            label={intl.formatMessage(messages.promptMithrilButton)}
            onClick={this.showConfirmation}
            skin={ButtonSkin}
          />
        </div>
      </div>
    );
  }

  renderConfirmView() {
    const { intl } = this.context;
    const { isStarting, startError } = this.state;

    return (
      <div className={styles.component}>
        <p className={styles.confirmBody}>
          {intl.formatMessage(messages.promptConfirmBody)}
        </p>
        {startError ? <div className={styles.error}>{startError}</div> : null}
        <div className={styles.actions}>
          <Button
            className={classNames([styles.actionButton])}
            label={intl.formatMessage(messages.promptConfirmCancel)}
            onClick={this.cancelConfirmation}
            skin={ButtonSkin}
          />
          <Button
            className={classNames([
              'primary',
              styles.actionButton,
              styles.primaryButton,
            ])}
            label={intl.formatMessage(messages.promptConfirmStart)}
            disabled={isStarting}
            onClick={this.handleStart}
            skin={ButtonSkin}
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
