// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import moment from 'moment';
import SVGInline from 'react-svg-inline';
import styles from './CountdownWidget.scss';
import delimeterIcon from '../../assets/images/delimeter.inline.svg';
import spinnerIcon from '../../assets/images/spinner.inline.svg';
import globalMessages from '../../i18n/global-messages';

const TIME_LEFT_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;
const COLUMNS = {
  YYYY: 'years',
  MM: 'months',
  DD: 'days',
  HH: 'hours',
  mm: 'minutes',
  ss: 'seconds',
};

type Format =
  | 'YYYY-MM-DD-HH-mm-ss'
  | 'MM-DD-HH-mm-ss'
  | 'DD-HH-mm-ss'
  | 'HH-mm-ss'
  | 'mm-ss'
  | 'YYYY'
  | 'MM'
  | 'DD'
  | 'HH'
  | 'mm'
  | 'ss';

type Props = {
  startDateTime: ?string,
  redirectOnEnd?: Function,
  format?: Format, // If no format is specified only non-empty columns will be shown
};

type State = { timeLeft: ?number };

@observer
export default class CountdownWidget extends Component<Props, State> {
  intervalHandler: ?IntervalID = null;
  state = { timeLeft: null };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidMount() {
    this.startTimer();
  }

  componentWillUnmount() {
    this.stopTimer();
  }

  startTimer = () => {
    this.updateTimeLeft();
    this.intervalHandler = setInterval(
      () => this.updateTimeLeft(),
      TIME_LEFT_INTERVAL
    );
  };

  stopTimer = () => {
    if (this.intervalHandler) {
      clearInterval(this.intervalHandler);
      this.intervalHandler = null;
    }
  };

  updateTimeLeft = () => {
    const { redirectOnEnd, startDateTime } = this.props;
    if (startDateTime) {
      const timeLeft = Math.max(
        0,
        new Date(startDateTime).getTime() - new Date().getTime()
      );

      this.setState({ timeLeft });

      if (timeLeft === 0 && redirectOnEnd) {
        redirectOnEnd();
      }
    }
  };

  generateFieldPanel = (labels: any, values: any, index: number) => {
    const value = values[index];
    const includeDelimeter = index !== values.length - 1;
    const labelStr = labels[index];
    const { format } = this.props;
    const shouldBeHidden =
      values.slice(0, index).reduce((acc, val) => acc + val, 0) === 0 &&
      value === 0 &&
      !format;
    if (shouldBeHidden) {
      return null;
    }

    const valueStr = value.toString();
    const zeroValue = valueStr.length === 1 ? '0' : '';
    const isZeroValue =
      valueStr === '0' &&
      values.slice(0, index).reduce((acc, val) => acc + val, 0) === 0;

    return (
      <div className={styles.fieldPanel}>
        <div className={styles.left}>
          <div className={styles.fieldLabel}>{labelStr}</div>
          <div className={styles.fieldValue}>
            {(isZeroValue || zeroValue) && <span>{zeroValue}</span>}
            {valueStr}
          </div>
        </div>
        {includeDelimeter && (
          <div className={styles.right}>
            <SVGInline svg={delimeterIcon} className={styles.delimeterIcon} />
          </div>
        )}
      </div>
    );
  };

  generateCountdownPanels = () => {
    const { intl } = this.context;
    const { timeLeft } = this.state;
    const { format } = this.props;
    const duration = moment.duration(timeLeft, 'milliseconds');

    const yearsLabel = intl.formatMessage(globalMessages.years);
    const monthsLabel = intl.formatMessage(globalMessages.months);
    const daysLabel = intl.formatMessage(globalMessages.days);
    const hoursLabel = intl.formatMessage(globalMessages.hours);
    const minutesLabel = intl.formatMessage(globalMessages.minutes);
    const secondsLabel = intl.formatMessage(globalMessages.seconds);
    const labels: Array<string> = format
      ? format
          .split('-')
          .map(item => intl.formatMessage(globalMessages[COLUMNS[item]]))
      : [
          yearsLabel,
          monthsLabel,
          daysLabel,
          hoursLabel,
          minutesLabel,
          secondsLabel,
        ];

    const years = duration.years();
    const months = duration.months();
    const days = duration.days();
    const hours = duration.hours();
    const minutes = duration.minutes();
    const seconds = duration.seconds();
    const values = format
      ? format.split('-').map(item => duration[COLUMNS[item]]())
      : [years, months, days, hours, minutes, seconds];
    const keys = format
      ? format.split('-').map(item => COLUMNS[item])
      : ['years', 'months', 'days', 'hours', 'minutes', 'seconds'];

    return labels.map<any>((
      label: string, // eslint-disable-line
      index: number
    ) => (
      <Fragment key={keys[index]}>
        {this.generateFieldPanel(labels, values, index)}
      </Fragment>
    ));
  };

  render() {
    const { startDateTime } = this.props;
    const fieldPanels = this.generateCountdownPanels();
    const showSpinner = startDateTime === null;
    return (
      <div className={styles.timeLeftContainer}>
        <div className={styles.timeLeft}>
          {showSpinner ? (
            <SVGInline svg={spinnerIcon} className={styles.spinnerIcon} />
          ) : (
            fieldPanels
          )}
        </div>
      </div>
    );
  }
}
