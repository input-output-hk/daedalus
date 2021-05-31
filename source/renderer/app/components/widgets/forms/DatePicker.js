// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { DateRange } from 'react-date-range';
import { Select } from 'react-polymorph/lib/components/Select';
import { Input } from 'react-polymorph/lib/components/Input';
import styles from './DatePicker.scss';
import TinyInput from './TinyInput';

const messages = defineMessages({
  dateRange: {
    id: 'widgets.datePicker.dateRange',
    defaultMessage: '!!!Date range',
    description: 'Date range of filter.',
  },
  selectTimeRange: {
    id: 'widgets.datePicker.selectTimeRange',
    defaultMessage: '!!!Select time range',
    description: 'Select time range indication of filter.',
  },
  last7Days: {
    id: 'widgets.datePicker.last7Days',
    defaultMessage: '!!!Last 7 days',
    description: 'Last 7 days range of filter.',
  },
  last30Days: {
    id: 'widgets.datePicker.last30Days',
    defaultMessage: '!!!Last 30 days',
    description: 'Last 30 days range of filter.',
  },
  last90Days: {
    id: 'widgets.datePicker.last90Days',
    defaultMessage: '!!!Last 90 days',
    description: 'Last 90 days range of filter.',
  },
  thisYear: {
    id: 'widgets.datePicker.thisYear',
    defaultMessage: '!!!This year',
    description: 'This year date range of filter.',
  },
  custom: {
    id: 'widgets.datePicker.custom',
    defaultMessage: '!!!Custom',
    description: 'Custom date range of filter.',
  },
});

export type DateRangeType =
  | ''
  | 'last7Days'
  | 'last30Days'
  | 'last90Days'
  | 'thisYear'
  | 'custom';

export const DateRangeTypes = {
  ALL_TIME: 'allTime',
  LAST_7_DAYS: 'last7Days',
  LAST_30_DAYS: 'last30Days',
  LAST_90_DAYS: 'last90Days',
  THIS_YEAR: 'thisYear',
  CUSTOM: 'custom',
};

type Props = {
  startDate: Date,
  endDate: ?Date,
  onChange: Function,
  currentLocale: string,
  currentDateFormat: string,
  initialSelectValue?: DateRangeType,
};

type State = {
  selectValue: DateRangeType,
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

export default class DatePicker extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    initialSelectValue: 'custom',
  };

  constructor(props: Props) {
    super(props);
    const { initialSelectValue } = this.props;
    this.state = {
      selectValue: initialSelectValue,
    };
  }

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

  get dateRangeOptions() {
    const { formatMessage } = this.context.intl;
    return [
      {
        label: 'All time',
        value: DateRangeTypes.ALL_TIME,
      },
      {
        label: formatMessage(messages.last7Days),
        value: DateRangeTypes.LAST_7_DAYS,
      },
      {
        label: formatMessage(messages.last30Days),
        value: DateRangeTypes.LAST_30_DAYS,
      },
      {
        label: formatMessage(messages.last90Days),
        value: DateRangeTypes.LAST_90_DAYS,
      },
      {
        label: formatMessage(messages.thisYear),
        value: DateRangeTypes.THIS_YEAR,
      },
      {
        label: formatMessage(messages.custom),
        value: DateRangeTypes.CUSTOM,
      },
    ];
  }

  inputSelectionRenderer = (a, b, c) => {
    console.log('a', a);
    console.log('b', b);
    console.log('c', c);
    return 'INPUT CUSTOM VALUE';
  };

  //   (input: 'from' | 'to') => {
  //
  //   }

  render() {
    const { intl } = this.context;
    const { startDate, endDate, currentLocale, currentDateFormat } = this.props;
    const { selectValue } = this.state;
    const ranges = [
      {
        startDate,
        endDate,
        key: 'selection',
      },
    ];
    return (
      <div className={styles.component}>
        <Select
          options={this.dateRangeOptions}
          value={selectValue}
          className={styles.dropdown}
          label={'Date rangez'}
          onChange={(selectValue) => {
            this.setState({ selectValue });
          }}
        />
        <TinyInput
          innerLabelPrefix="from"
          innerValue="01/01/2021"
          value="01/01/2021"
          onChange={(v) => console.log(v)}
          autoFocus={false}
          onSubmit={(a, b, c) => {
            console.log('SUBMIT ---');
            console.log('a', a);
            console.log('b', b);
            console.log('c', c);
          }}
          onFocus={() => console.log('onFocus')}
          onBlur={() => console.log('onBlur')}
          onClick={() => console.log('onClick')}
          useReadMode
          readOnly
        />
        {selectValue === DateRangeTypes.CUSTOM && (
          <div className={styles.calendar}>
            <DateRange
              onChange={this.handleChange}
              moveRangeOnFirstSelection={false}
              ranges={ranges}
              locale1={locales[currentLocale]}
              dateDisplayFormat={dateFormats[currentDateFormat]}
            />
          </div>
        )}
      </div>
    );
  }
}
