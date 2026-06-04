import React, { Component } from 'react';
import classNames from 'classnames';
import { intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';

import { ALLOWED_TIME_DIFFERENCE } from '../../config/timingConfig';
import globalMessages from '../../i18n/global-messages';
import { formattedNumber } from '../../utils/formatters';
import sandClockIcon from '../../assets/images/sand-clock-xs.inline.svg';
import styles from './DaedalusDiagnostics.scss';

type Props = {
  isCheckingSystemTime: boolean;
  isForceCheckingSystemTime: boolean;
  isNodeResponding: boolean;
  localTimeDifference: number | null | undefined;
  onCheckTime: () => void;
};

export default class DiagnosticsTimeStatusRow extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      isCheckingSystemTime,
      isForceCheckingSystemTime,
      isNodeResponding,
      localTimeDifference,
      onCheckTime,
    } = this.props;
    const { intl } = this.context;
    const isNTPServiceReachable = localTimeDifference != null;
    const localTimeDifferenceClasses = isCheckingSystemTime
      ? classNames([styles.layoutData, styles.localTimeDifference])
      : classNames([
          styles.layoutData,
          styles.localTimeDifference,
          !isNTPServiceReachable ||
          (localTimeDifference &&
            Math.abs(localTimeDifference) > ALLOWED_TIME_DIFFERENCE)
            ? styles.red
            : styles.green,
        ]);

    return (
      <div className={styles.layoutRow}>
        <div className={styles.layoutHeader}>
          {intl.formatMessage({
            id: 'daedalus.diagnostics.dialog.localTimeDifference',
            defaultMessage: '!!!Local time difference',
          })}
          {intl.formatMessage(globalMessages.punctuationColon)}
        </div>
        <div className={localTimeDifferenceClasses}>
          <button
            onClick={onCheckTime}
            disabled={isForceCheckingSystemTime || !isNodeResponding}
          >
            {isForceCheckingSystemTime
              ? intl.formatMessage({
                  id: 'daedalus.diagnostics.dialog.localTimeDifferenceChecking',
                  defaultMessage: '!!!Checking...',
                })
              : intl.formatMessage({
                  id: 'daedalus.diagnostics.dialog.localTimeDifferenceCheckTime',
                  defaultMessage: '!!!Check time',
                })}
          </button>
          {isCheckingSystemTime ? (
            <span className={localTimeDifferenceClasses}>
              <SVGInline
                svg={sandClockIcon}
                className={styles.networkTipSandClock}
              />
            </span>
          ) : (
            <span className={localTimeDifferenceClasses}>
              {isNTPServiceReachable
                ? `${formattedNumber(localTimeDifference || 0)} μs`
                : intl.formatMessage({
                    id: 'daedalus.diagnostics.dialog.serviceUnreachable',
                    defaultMessage: '!!!Service unreachable',
                  })}
            </span>
          )}
        </div>
      </div>
    );
  }
}
