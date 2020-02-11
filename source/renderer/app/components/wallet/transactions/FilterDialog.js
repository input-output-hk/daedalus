// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import type { ElementRef } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { i18nContext } from '../../../utils/i18nContext';
import {
  calculateDateRange,
  formatAmountValue,
  validateFilterForm,
} from '../../../utils/transaction';
import type { TransactionFilterOptionsType } from '../../../stores/TransactionsStore';
import {
  DateRangeTypes,
  emptyTransactionFilterOptions,
} from '../../../stores/TransactionsStore';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinySelect from '../../widgets/forms/TinySelect';
import TinyInput from '../../widgets/forms/TinyInput';
import TinyDatePicker from '../../widgets/forms/TinyDatePicker';
import TinyButton from '../../widgets/forms/TinyButton';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterDialog.scss';

const messages = defineMessages({
  incoming: {
    id: 'wallet.transaction.filter.incoming',
    defaultMessage: '!!!Ada received',
    description: 'Incoming filter type.',
  },
  outgoing: {
    id: 'wallet.transaction.filter.outgoing',
    defaultMessage: '!!!Ada sent',
    description: 'Outgoing filter type.',
  },
  dateRange: {
    id: 'wallet.transaction.filter.dateRange',
    defaultMessage: '!!!Time',
    description: 'Date range of filter.',
  },
  anyTime: {
    id: 'wallet.transaction.filter.anyTime',
    defaultMessage: '!!!Any time',
    description: 'All date range of filter.',
  },
  last7Days: {
    id: 'wallet.transaction.filter.last7Days',
    defaultMessage: '!!!Last 7 days',
    description: 'Last 7 days range of filter.',
  },
  last30Days: {
    id: 'wallet.transaction.filter.last30Days',
    defaultMessage: '!!!Last 30 days',
    description: 'Last 30 days range of filter.',
  },
  last90Days: {
    id: 'wallet.transaction.filter.last90Days',
    defaultMessage: '!!!Last 90 days',
    description: 'Last 90 days range of filter.',
  },
  thisYear: {
    id: 'wallet.transaction.filter.thisYear',
    defaultMessage: '!!!This year',
    description: 'This year date range of filter.',
  },
  custom: {
    id: 'wallet.transaction.filter.custom',
    defaultMessage: '!!!Custom',
    description: 'Custom date range of filter.',
  },
  amountRange: {
    id: 'wallet.transaction.filter.amountRange',
    defaultMessage: '!!!Amount of ada',
    description: 'Amount range of filter.',
  },
  apply: {
    id: 'wallet.transaction.filter.apply',
    defaultMessage: '!!!Apply',
    description: 'Filter button label.',
  },
});

const FILTER_PANEL_OFFSET = 103;
const FILTER_DIALOG_WITH_DATE_PICKER_HEIGHT = 521;

const applyDialogStyles = () => {
  const dialogElement = window.document.querySelector('.ReactModal__Content');
  const dialogOverlayElement = dialogElement.parentElement;
  const sidebarLayoutContentWrapper = window.document.querySelector(
    '.SidebarLayout_contentWrapper'
  );
  const filterButtonElement = window.document.querySelector(
    '.FilterButton_component'
  );
  const windowHeight = window.document.body.clientHeight;
  const filterDialogHeight = dialogElement.clientHeight;
  const filterDialogOffsetTop =
    sidebarLayoutContentWrapper.offsetTop + FILTER_PANEL_OFFSET;

  dialogOverlayElement.style.backgroundColor = 'transparent';
  dialogElement.style.backgroundColor =
    'var(--theme-transactions-filter-modal-bg-color)';
  dialogElement.style.borderRadius = '4px';
  dialogElement.style.minWidth = 'auto';
  dialogElement.style.position = 'absolute';

  if (windowHeight - filterDialogOffsetTop < filterDialogHeight) {
    dialogElement.style.right = `${filterButtonElement.clientWidth + 20}px`;
    dialogElement.style.top = `${Math.max(
      sidebarLayoutContentWrapper.offsetTop + 75 - filterDialogHeight / 2,
      40
    )}px`;
  } else {
    dialogElement.style.right = '10px';
    dialogElement.style.top = `${filterDialogOffsetTop}px`;
  }
  if (
    dialogElement.offsetTop + FILTER_DIALOG_WITH_DATE_PICKER_HEIGHT >=
    windowHeight
  ) {
    dialogElement.children[0].classList.add(['small-height-for-date-picker']);
  } else {
    dialogElement.children[0].classList.remove([
      'small-height-for-date-picker',
    ]);
  }
};

type Props = {
  locale: string,
  dateFormat: string,
  defaultFilterOptions: TransactionFilterOptionsType,
  populatedFilterOptions: TransactionFilterOptionsType,
  onFilter: Function,
  onClose: Function,
};

@observer
export default class FilterDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  dateRangeOptions: Array<{ label: string, value: string }>;
  form: ReactToolboxMobxForm;

  constructor(props: Props) {
    super(props);

    const {
      locale,
      populatedFilterOptions: {
        incomingChecked,
        outgoingChecked,
        dateRange,
        fromDate,
        toDate,
        fromAmount,
        toAmount,
      },
    } = props;
    const intl = i18nContext(locale);

    this.dateRangeOptions = [
      {
        label: intl.formatMessage(messages.anyTime),
        value: DateRangeTypes.ANY_TIME,
      },
      {
        label: intl.formatMessage(messages.last7Days),
        value: DateRangeTypes.LAST_7_DAYS,
      },
      {
        label: intl.formatMessage(messages.last30Days),
        value: DateRangeTypes.LAST_30_DAYS,
      },
      {
        label: intl.formatMessage(messages.last90Days),
        value: DateRangeTypes.LAST_90_DAYS,
      },
      {
        label: intl.formatMessage(messages.thisYear),
        value: DateRangeTypes.THIS_YEAR,
      },
      {
        label: intl.formatMessage(messages.custom),
        value: DateRangeTypes.CUSTOM,
      },
    ];
    this.form = new ReactToolboxMobxForm({
      fields: {
        incomingChecked: {
          type: 'checkbox',
          label: intl.formatMessage(messages.incoming),
          value: incomingChecked,
        },
        outgoingChecked: {
          type: 'checkbox',
          label: intl.formatMessage(messages.outgoing),
          value: outgoingChecked,
        },
        dateRange: {
          label: intl.formatMessage(messages.dateRange),
          value: dateRange,
        },
        fromDate: {
          label: '',
          value: fromDate,
        },
        toDate: {
          label: '',
          value: toDate,
        },
        fromAmount: {
          type: 'number',
          label: '',
          value: fromAmount,
        },
        toAmount: {
          type: 'number',
          label: '',
          value: toAmount,
        },
      },
    });
  }

  componentDidMount() {
    window.addEventListener('resize', applyDialogStyles);
  }

  componentWillUnmount() {
    window.removeEventListener('resize', applyDialogStyles);
  }

  handleSelfRef = (ref: ?ElementRef<'div'>) => {
    if (ref) {
      applyDialogStyles();
    }
  };

  setFilterType = (
    field: 'incomingChecked' | 'outgoingChecked',
    value: boolean
  ) => {
    this.form.select(field).set(value);
    if (value === false) {
      const otherFieldName =
        field === 'incomingChecked' ? 'outgoingChecked' : 'incomingChecked';
      const otherField = this.form.select(otherFieldName);
      if (otherField.value === false) {
        otherField.set(true);
      }
    }
  };

  resetForm = () => {
    const {
      dateRange,
      fromDate,
      toDate,
      fromAmount,
      toAmount,
      incomingChecked,
      outgoingChecked,
    } = emptyTransactionFilterOptions;

    this.form.select('fromDate').set(fromDate);
    this.form.select('toDate').set(toDate);
    this.form.select('dateRange').set(dateRange);
    this.form.select('fromAmount').set(fromAmount);
    this.form.select('toAmount').set(toAmount);
    this.form.select('incomingChecked').set(incomingChecked);
    this.form.select('outgoingChecked').set(outgoingChecked);
  };

  handleSubmit = () =>
    this.form.submit({
      onSuccess: form => {
        const { onFilter } = this.props;
        const { dateRange, fromDate, toDate, ...rest } = form.values();
        if (validateFilterForm(form.values()).isValid) {
          const dateRangePayload = calculateDateRange(dateRange, {
            fromDate,
            toDate,
          });

          onFilter({
            ...rest,
            ...dateRangePayload,
            dateRange,
          });
        }
      },
      onError: () => null,
    });

  isValidFromDate = (date: Object) => {
    return date.isSameOrBefore(moment().endOf('day'));
  };

  isValidToDate = (date: Object) => {
    const { fromDate } = this.form.values();
    return (
      date.isSameOrBefore(moment().endOf('day')) &&
      date.isSameOrAfter(moment(fromDate).startOf('day'))
    );
  };

  renderTypeField = () => {
    const { form } = this;
    const incomingCheckboxField = form.$('incomingChecked');
    const outgoingCheckboxField = form.$('outgoingChecked');

    return (
      <div className={styles.type}>
        <div className={styles.body}>
          <div className={styles.typeCheckbox}>
            <TinyCheckbox
              {...incomingCheckboxField.bind()}
              onChange={isSelected =>
                this.setFilterType('incomingChecked', isSelected)
              }
            />
          </div>
          <div className={styles.typeCheckbox}>
            <TinyCheckbox
              {...outgoingCheckboxField.bind()}
              onChange={isSelected =>
                this.setFilterType('outgoingChecked', isSelected)
              }
            />
          </div>
        </div>
      </div>
    );
  };

  renderDateRangeField = () => {
    const dateRangeField = this.form.$('dateRange');

    return (
      <div className={styles.dateRange}>
        <TinySelect
          {...dateRangeField.bind()}
          options={this.dateRangeOptions}
        />
      </div>
    );
  };

  renderDateRangeFromToField = () => {
    const { form } = this;
    const { intl } = this.context;
    const {
      locale,
      dateFormat,
      defaultFilterOptions: { fromDate, toDate },
    } = this.props;
    const { invalidFields } = validateFilterForm(form.values());
    const defaultFromDate = fromDate || '1970-01-01';
    const defaultToDate = toDate || moment().format('YYYY-MM-DD');
    const fromDateField = form.$('fromDate');
    const toDateField = form.$('toDate');
    const fromDateClassNames = classNames([
      styles.dateRangeInput,
      styles.fromDateInput,
    ]);
    const toDateClassNames = classNames([
      styles.dateRangeInput,
      styles.toDateInput,
    ]);

    return (
      <div className={styles.dateRangeFromTo}>
        <div className={styles.body}>
          <div className={fromDateClassNames}>
            <TinyDatePicker
              {...fromDateField.bind()}
              label={intl.formatMessage(globalMessages.rangeFrom)}
              placeholder={moment(defaultFromDate).format(dateFormat)}
              pickerPanelPosition="left"
              closeOnSelect
              onReset={() => form.select('fromDate').set('')}
              isValidDate={this.isValidFromDate}
              locale={locale}
              dateFormat={dateFormat}
            />
          </div>
          <div className={toDateClassNames}>
            <TinyDatePicker
              {...toDateField.bind()}
              label={intl.formatMessage(globalMessages.rangeTo)}
              placeholder={moment(defaultToDate).format(dateFormat)}
              pickerPanelPosition="right"
              closeOnSelect
              onReset={() => form.select('toDate').set('')}
              isValidDate={this.isValidToDate}
              locale={locale}
              dateFormat={dateFormat}
              error={invalidFields.toDate}
            />
          </div>
        </div>
      </div>
    );
  };

  onAmountFieldBlur = (selector: string) => {
    const { form } = this;
    const { fromAmount, toAmount } = form.values();

    if (selector === 'fromAmount' && fromAmount) {
      if (Number.isNaN(Number(fromAmount))) {
        form.select(selector).set('');
      } else {
        const fromValue = new BigNumber(fromAmount).toString();
        form.select(selector).set(fromValue);
      }
    } else if (selector === 'toAmount' && toAmount) {
      if (Number.isNaN(Number(toAmount))) {
        form.select(selector).set('');
      } else {
        const toValue = new BigNumber(toAmount).toString();
        form.select(selector).set(toValue);
      }
    }
  };

  renderAmountRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const {
      defaultFilterOptions: { fromAmount, toAmount },
    } = this.props;
    const { invalidFields } = validateFilterForm(form.values());
    const defaultFromAmount = fromAmount || '0';
    const defaultToAmount = toAmount || '10000';
    const fromAmountField = form.$('fromAmount');
    const toAmountField = form.$('toAmount');
    const { fromAmount: fromValue, toAmount: toValue } = form.values();
    const fromAmountInnerValue = formatAmountValue(
      fromValue,
      defaultFromAmount
    );
    const toAmountInnerValue = formatAmountValue(toValue, defaultToAmount);
    const fromAmountFieldProps = fromAmountField.bind();
    const toAmountFieldProps = toAmountField.bind();

    return (
      <div className={styles.amountRange}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.amountRange)}</label>
        </div>
        <div className={styles.body}>
          <div className={styles.amountRangeInput}>
            <TinyInput
              {...fromAmountFieldProps}
              onBlur={(evt: SyntheticEvent<EventTarget>) => {
                fromAmountFieldProps.onBlur(evt);
                this.onAmountFieldBlur('fromAmount');
              }}
              onSubmit={this.handleSubmit}
              useReadMode
              notNegative
              digitCountAfterDecimalPoint={6}
              maxLength={20}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeFrom)}
              innerLabelSuffix={intl.formatMessage(globalMessages.unitAda)}
              innerValue={fromAmountInnerValue}
            />
          </div>
          <div className={styles.amountRangeInput}>
            <TinyInput
              {...toAmountFieldProps}
              onBlur={(evt: SyntheticEvent<EventTarget>) => {
                toAmountFieldProps.onBlur(evt);
                this.onAmountFieldBlur('toAmount');
              }}
              onSubmit={this.handleSubmit}
              useReadMode
              notNegative
              digitCountAfterDecimalPoint={6}
              maxLength={20}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeTo)}
              innerLabelSuffix={intl.formatMessage(globalMessages.unitAda)}
              innerValue={toAmountInnerValue}
              error={invalidFields.toAmount}
            />
          </div>
        </div>
      </div>
    );
  };

  renderActionButton = () => {
    const { intl } = this.context;
    const { isValid } = validateFilterForm(this.form.values());

    return (
      <div className={styles.action}>
        <TinyButton
          label={intl.formatMessage(messages.apply)}
          loading={false}
          disabled={!isValid}
          onClick={this.handleSubmit}
        />
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { onClose } = this.props;

    return (
      <Dialog
        closeOnOverlayClick
        className={styles.component}
        onClose={onClose}
      >
        <div ref={this.handleSelfRef}>
          <div className={styles.title}>
            <h4 className={styles.titleText}>
              {intl.formatMessage(globalMessages.filter)}
            </h4>
            <button className={styles.titleLink} onClick={this.resetForm}>
              {intl.formatMessage(globalMessages.reset)}
            </button>
          </div>
          <div className={styles.content}>
            {this.renderTypeField()}
            {this.renderDateRangeField()}
            {this.renderDateRangeFromToField()}
            {this.renderAmountRangeField()}
            {this.renderActionButton()}
          </div>
        </div>
      </Dialog>
    );
  }
}
