// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import { DateRange as DatePicker } from 'react-date-range';
import { Select } from 'react-polymorph/lib/components/Select';
import { Input } from 'react-polymorph/lib/components/Input';
import styles from './DateRange.scss';

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
  from: {
    id: 'widgets.datePicker.from',
    defaultMessage: '!!!From',
    description: 'from date range of filter.',
  },
  to: {
    id: 'widgets.datePicker.to',
    defaultMessage: '!!!To',
    description: 'to date range of filter.',
  },
});

export type DateRangeType =
  | 'allTime'
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

export type InputType = 'from' | 'to';

const defaultOption = DateRangeTypes.LAST_30_DAYS;
// const defaultDateFrom = new Date();

type Props = {
  onChange?: Function,
  currentLocale: string,
  currentDateFormat: string,
  initialSelectValue?: DateRangeType,
  // TODO:
  // initialDateFrom?: ?Date,
  // initialDateTo?: ?Date,
  // maxDateRange:
  label?: string,
};

type State = {
  selectValue: DateRangeType,
  dateFrom: Date,
  dateTo?: Date,
  editingField: InputType,
  isCalendarOpen: boolean,
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

export default class DateRange extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: Props) {
    super(props);
    const { initialSelectValue } = this.props;
    const dateFrom = this.getDateFromRange(initialSelectValue || defaultOption);
    const dateTo = new Date();
    this.state = {
      editingField: 'from',
      isCalendarOpen: false,
      selectValue: initialSelectValue || defaultOption,
      dateFrom,
      dateTo,
    };
  }

  handleDatePickerChange = (newState: {
    selection: { startDate: Date, endDate: Date },
  }) => {
    const { editingField: currentEditingField } = this.state;
    let editingField;
    if (currentEditingField === 'to') {
      editingField = 'from';
    } else {
      editingField = 'to';
    }
    const { onChange } = this.props;
    const { startDate, endDate } = newState.selection;
    const isCalendarOpen = editingField === 'from';
    this.setState({
      dateFrom: startDate,
      dateTo: endDate,
      editingField,
      isCalendarOpen,
    });
    if (!editingField && onChange) {
      onChange({
        dateFrom: startDate,
        dateTo: endDate,
      });
    }
  };

  handleSelectChange = (selectValue: DateRangeType) => {
    if (selectValue === DateRangeTypes.CUSTOM) {
      return this.setCustomRange();
    }
    const dateFrom = this.getDateFromRange(selectValue);
    const dateTo = new Date();
    this.setState({
      selectValue,
      dateFrom,
      dateTo,
    });
    return null;
  };

  setType = (selectValue: DateRangeType) => {
    this.setState({
      selectValue,
    });
  };

  getDateFromRange = (range: DateRangeType): Date => {
    let dateFrom = moment();
    if (range === DateRangeTypes.ALL_TIME) {
      dateFrom = moment(dateFrom).subtract(1, 'months');
    } else if (range === DateRangeTypes.LAST_7_DAYS) {
      dateFrom = moment(dateFrom).subtract(7, 'days');
    } else if (range === DateRangeTypes.LAST_30_DAYS) {
      dateFrom = moment(dateFrom).subtract(30, 'days');
    } else if (range === DateRangeTypes.LAST_90_DAYS) {
      dateFrom = moment(dateFrom).subtract(90, 'days');
    } else if (range === DateRangeTypes.THIS_YEAR) {
      dateFrom = moment(dateFrom).subtract(1, 'year');
    }
    return dateFrom.toDate();
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

  inputSelectionRenderer = (type: InputType) => {
    const { intl } = this.context;
    const { currentDateFormat } = this.props;
    const { dateFrom, dateTo } = this.state;
    const prefixMessage = messages[type];
    const date = type === 'from' ? dateFrom : dateTo;
    const value = moment(date).isValid()
      ? moment(date).format(currentDateFormat)
      : '-';
    return (
      <div className={styles.inputContent}>
        <span>{intl.formatMessage(prefixMessage)}</span>
        &nbsp;
        {value}
      </div>
    );
  };

  setCustomRange = () => {
    const { selectValue } = this.state;
    let { dateFrom, dateTo } = this.state;
    if (
      selectValue === DateRangeTypes.LAST_90_DAYS ||
      selectValue === DateRangeTypes.THIS_YEAR
    ) {
      dateFrom = new Date();
      dateTo = new Date();
    }
    const isCalendarOpen = true;
    this.setState({
      selectValue: DateRangeTypes.CUSTOM,
      dateFrom,
      dateTo,
      isCalendarOpen,
    });
  };

  render() {
    // const { intl } = this.context;
    const { currentLocale, currentDateFormat, label } = this.props;
    const { selectValue, dateFrom, dateTo, isCalendarOpen } = this.state;
    const ranges = [
      {
        startDate: dateFrom,
        endDate: dateTo,
        key: 'selection',
      },
    ];
    return (
      <div className={styles.component}>
        <Select
          options={this.dateRangeOptions}
          value={selectValue}
          className={styles.dropdown}
          label={label}
          onChange={this.handleSelectChange}
        />

        <div className={styles.inputs}>
          <Input
            selectedOption="from"
            className={styles.inputFrom}
            selectionRenderer={this.inputSelectionRenderer}
            readOnly
            onClick={this.setCustomRange}
          />
          <Input
            selectedOption="to"
            className={styles.inputTo}
            selectionRenderer={this.inputSelectionRenderer}
            readOnly
            onClick={this.setCustomRange}
          />
        </div>
        {selectValue === DateRangeTypes.CUSTOM && isCalendarOpen && (
          <div className={styles.calendar}>
            <DatePicker
              onChange={this.handleDatePickerChange}
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
