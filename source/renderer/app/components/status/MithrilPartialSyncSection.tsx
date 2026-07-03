import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';

import globalMessages from '../../i18n/global-messages';
import { logger } from '../../utils/logging';
import { getMithrilStartErrorMessage } from '../../utils/mithrilErrorMessage';
import MithrilPartialSyncConfirmation from './MithrilPartialSyncConfirmation';
import MithrilPartialSyncRecommendation from './MithrilPartialSyncRecommendation';
import type { MithrilAvailabilityVariant } from './MithrilPartialSyncRecommendation';
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
  isSignificantlyBehind: boolean;
  isProbeFailed: boolean;
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
        startError: getMithrilStartErrorMessage(error, this.context.intl),
      });
      this.props.onRestoreFocus();
    }
  };

  render() {
    const {
      isActionBlocked,
      isSignificantlyBehind,
      isProbeFailed,
    } = this.props;
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

    // The section stays visible in every probe state; only the tooltip copy
    // adapts. A confident behind result outranks the failure hint, and a
    // failed probe outranks the near-tip reassurance.
    let availabilityVariant: MithrilAvailabilityVariant = 'near-tip';
    if (isSignificantlyBehind) {
      availabilityVariant = 'behind';
    } else if (isProbeFailed) {
      availabilityVariant = 'availability-unknown';
    }

    return (
      <div className={styles.layoutRow}>
        <div className={styles.layoutHeader}>
          {intl.formatMessage(messages.sectionLabel)}
          {intl.formatMessage(globalMessages.punctuationColon)}
        </div>
        <MithrilPartialSyncRecommendation
          isActionBlocked={isActionBlocked}
          variant={availabilityVariant}
          onShowConfirmation={this.showConfirmation}
        />
      </div>
    );
  }
}
