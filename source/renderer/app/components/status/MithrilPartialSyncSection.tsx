import React, { Component } from 'react';
import { intlShape } from 'react-intl';

import globalMessages from '../../i18n/global-messages';
import { logger } from '../../utils/logging';
import MithrilPartialSyncConfirmation from './MithrilPartialSyncConfirmation';
import MithrilPartialSyncRecommendation from './MithrilPartialSyncRecommendation';
import styles from './DaedalusDiagnostics.scss';

type Props = {
  formattedSyncPercentage: string;
  isActionBlocked: boolean;
  isMithrilPartialSyncWorking: boolean;
  isSynced: boolean;
  shouldShowRecommendation: boolean;
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
    const {
      formattedSyncPercentage,
      isActionBlocked,
      isSynced,
      shouldShowRecommendation,
    } = this.props;
    const { isShowingConfirmation, startError } = this.state;
    const { intl } = this.context;

    if (isShowingConfirmation) {
      return (
        <MithrilPartialSyncConfirmation
          isActionBlocked={isActionBlocked}
          startError={startError}
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
          Mithril Partial Sync
          {intl.formatMessage(globalMessages.punctuationColon)}
        </div>
        <MithrilPartialSyncRecommendation
          formattedSyncPercentage={formattedSyncPercentage}
          isActionBlocked={isActionBlocked}
          isSynced={isSynced}
          onShowConfirmation={this.showConfirmation}
        />
      </div>
    );
  }
}
