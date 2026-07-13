import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';

import { logger } from '../../../utils/logging';
import { getMithrilStartErrorMessage } from '../../../utils/mithrilErrorMessage';
// Shared summary sentence, imported so the confirm view and the diagnostics modal stay identical; do not redeclare the id locally.
import mithrilSyncProcessSummaryMessages from '../../status/MithrilSyncProcessSummary.messages';
import styles from './SyncingConnectingMithrilPrompt.scss';

const messages = defineMessages({
  promptTitle: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptTitle',
    defaultMessage: '!!!Mithril Sync',
    description:
      'Choice-view title for the proactive Mithril prompt on the syncing screen',
  },
  promptBody: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptBody',
    defaultMessage:
      '!!!Your node is about {epochs, plural, one {# epoch} other {# epochs}} behind.',
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
    defaultMessage: '!!!Mithril can catch you up faster than Blockchain Sync.',
    description:
      'Benefit-vs-standard-sync body line (line 2) for the proactive Mithril fork prompt',
  },
  promptHandoffNote: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote',
    defaultMessage:
      '!!!If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. ({shortcut})',
    description:
      'Handoff note telling the user they can start Mithril later from Diagnostics if they skip the proactive prompt',
  },
  promptHandoffNoteLabel: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNoteLabel',
    defaultMessage: '!!!Note:',
    description:
      'Label for the proactive Mithril prompt handoff note line on the syncing screen',
  },
  promptMithrilButton: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptMithrilButton',
    defaultMessage: '!!!Mithril Sync (fast)',
    description:
      'Primary choice-view button label that opens the concise inline confirm step for the proactive Mithril fork prompt',
  },
  promptStandardButton: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptStandardButton',
    defaultMessage: '!!!Blockchain Sync (slow)',
    description:
      'Secondary choice-view button label that dismisses the proactive Mithril fork prompt for this session',
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
  promptConfirmTitle: {
    id: 'daedalus.diagnostics.dialog.mithrilProactivePromptConfirmTitle',
    defaultMessage: '!!!Mithril Sync Process',
    description:
      'Confirm-view title for the proactive Mithril prompt on the syncing screen',
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

  // The choice-view button only reveals the confirm view; handleStart is the single caller of onStart, so confirmation always precedes start.
  handleStart = async () => {
    this.setState({ isStarting: true, startError: null });
    try {
      await this.props.onStart();
      // On success the store flips to a working status and the overlay takes over, unmounting this prompt; isStarting disables the button so a lingering mount cannot start twice.
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
        startError: getMithrilStartErrorMessage(error, this.context.intl),
      });
    }
  };

  renderChoiceView() {
    const { behindByEpochs, onDismiss } = this.props;
    const { intl } = this.context;
    const { isMacOS } = global.environment;

    const hasBehindFigure =
      typeof behindByEpochs === 'number' && Number.isFinite(behindByEpochs);

    return (
      <div className={styles.component}>
        <h1 className={styles.title}>
          {intl.formatMessage(messages.promptTitle)}
        </h1>
        <p className={styles.body}>
          <span>
            {hasBehindFigure
              ? intl.formatMessage(messages.promptBody, {
                  epochs: behindByEpochs,
                })
              : intl.formatMessage(messages.promptBodyUnknown)}
          </span>{' '}
          <span>{intl.formatMessage(messages.promptBodyBenefit)}</span>
        </p>
        <p className={styles.handoffNote}>
          <strong className={styles.noteLabel}>
            {intl.formatMessage(messages.promptHandoffNoteLabel)}
          </strong>{' '}
          <span>
            {intl.formatMessage(messages.promptHandoffNote, {
              shortcut: isMacOS ? 'Cmd + D' : 'Ctrl + D',
            })}
          </span>
        </p>
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            label={intl.formatMessage(messages.promptStandardButton)}
            onClick={onDismiss}
            skin={ButtonSkin}
          />
          {/* "Mithril Sync (fast)" is the primary/right (default) action. The
              visible-highlight contrast across themes is a theme-token / .scss
              concern; no scss change here. */}
          <Button
            className={styles.primaryAction}
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
        <h1 className={styles.title}>
          {intl.formatMessage(messages.promptConfirmTitle)}
        </h1>
        <p className={styles.confirmBody}>
          {intl.formatMessage(mithrilSyncProcessSummaryMessages.processSummary)}
        </p>
        {startError ? <div className={styles.error}>{startError}</div> : null}
        <div className={styles.actions}>
          <Button
            className={styles.secondaryAction}
            label={intl.formatMessage(messages.promptConfirmCancel)}
            onClick={this.cancelConfirmation}
            skin={ButtonSkin}
          />
          <Button
            className={styles.primaryAction}
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
