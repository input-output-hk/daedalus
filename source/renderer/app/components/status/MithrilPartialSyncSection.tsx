import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';

import globalMessages from '../../i18n/global-messages';
import { logger } from '../../utils/logging';
import MithrilPartialSyncConfirmation from './MithrilPartialSyncConfirmation';
import MithrilPartialSyncRecommendation from './MithrilPartialSyncRecommendation';
import styles from './DaedalusDiagnostics.scss';

const messages = defineMessages({
  sectionLabel: {
    id: 'daedalus.diagnostics.dialog.mithrilPartialSyncSectionLabel',
    defaultMessage: '!!!Mithril Sync',
    description:
      'Row label for the Mithril partial sync section in the diagnostics dialog',
  },
});

type Props = {
  isActionBlocked: boolean;
  isMithrilPartialSyncWorking: boolean;
  shouldShowRecommendation: boolean;
  behindByEpochs?: number;
  showConfirmationOnOpen?: boolean;
  onRestoreFocus: () => void;
  onStartMithrilPartialSync: (...args: Array<any>) => any;
};

type State = {
  isShowingConfirmation: boolean;
  startError: string | null;
};

export default class MithrilPartialSyncSection extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isShowingConfirmation: false,
    startError: null,
  };
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    if (this.props.showConfirmationOnOpen && !this.props.isActionBlocked) {
      this.showConfirmation();
    }
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  componentDidUpdate(prevProps: Props) {
    if (
      this.state.isShowingConfirmation &&
      !prevProps.isMithrilPartialSyncWorking &&
      this.props.isMithrilPartialSyncWorking
    ) {
      this.hideConfirmation();
    }
  }

  showConfirmation = () => {
    if (this.props.isActionBlocked) {
      return;
    }

    this.setState({
      isShowingConfirmation: true,
      startError: null,
    });
    this.props.onRestoreFocus();
  };

  hideConfirmation = () => {
    if (!this._isMounted) {
      return;
    }

    this.setState({
      isShowingConfirmation: false,
      startError: null,
    });
    this.props.onRestoreFocus();
  };

  startFromConfirmation = async () => {
    try {
      await this.props.onStartMithrilPartialSync();
      this.hideConfirmation();
    } catch (error) {
      logger.warn(
        'MithrilPartialSyncSection: Mithril partial sync start rejected after confirmation',
        { error }
      );
      if (!this._isMounted) {
        return;
      }

      this.setState({
        startError:
          error instanceof Error
            ? error.message
            : 'Unable to start Mithril partial sync.',
      });
      this.props.onRestoreFocus();
    }
  };

  render() {
    const { isActionBlocked, shouldShowRecommendation } = this.props;
    const { isShowingConfirmation, startError } = this.state;
    const { intl } = this.context;

    if (isShowingConfirmation) {
      return (
        <MithrilPartialSyncConfirmation
          isActionBlocked={isActionBlocked}
          startError={startError}
          behindByEpochs={this.props.behindByEpochs}
          onCancel={this.hideConfirmation}
          onConfirm={this.startFromConfirmation}
        />
      );
    }

    if (!shouldShowRecommendation) {
      return null;
    }

    return (
      <div className={styles.layoutRow}>
        <div className={styles.layoutHeader}>
          {intl.formatMessage(messages.sectionLabel)}
          {intl.formatMessage(globalMessages.punctuationColon)}
        </div>
        <MithrilPartialSyncRecommendation
          isActionBlocked={isActionBlocked}
          onShowConfirmation={this.showConfirmation}
        />
      </div>
    );
  }
}
