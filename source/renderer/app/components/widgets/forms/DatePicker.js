// @flow
import React, { Component } from 'react';
import { DateRange } from 'react-date-range';
import { intlShape } from 'react-intl';
import styles from './DatePicker.scss';

type Props = {
  startDate: Date,
  endDate: ?Date,
  onChange: Function,
  currentLocale: string,
  currentDateFormat: string,
  // currenTheme: string,
};

const locales = {
  ['en-US']: 'enUS',
  ['ja-JP']: 'ja',
};

const dateFormats = {
  ['MM/DD/YYYY']: 'MM/dd/yyyy',
  ['DD/MM/YYYY']: 'dd/MM/yyyy',
  ['YYYY/MM/DD']: 'yyyy-MM-dd',
};

export default class DatePicker extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleChange = (newState: {
    selection: { startDate: Date, endDate: Date },
  }) => {
    const { onChange } = this.props;
    const { startDate, endDate } = newState.selection;
    onChange({
      startDate,
      endDate,
    });
  };

  render() {
    const { startDate, endDate, currentLocale, currentDateFormat } = this.props;
    const ranges = [
      {
        startDate,
        endDate,
        key: 'selection',
      },
    ];
    return (
      <div className={styles.component}>
        <DateRange
          onChange={this.handleChange}
          moveRangeOnFirstSelection={false}
          ranges={ranges}
          locale1={locales[currentLocale]}
          dateDisplayFormat={dateFormats[currentDateFormat]}
        />
      </div>
    );
  }
}
