// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import type {
  DateRangeType,
  TransactionFilterOptionsType,
} from '../../../stores/TransactionsStore';
import {
  DateRangeTypes,
  emptyTransactionFilterOptions,
} from '../../../stores/TransactionsStore';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinySelect from '../../widgets/forms/TinySelect';
import TinyInput from '../../widgets/forms/TinyInput';
import TinyDatePicker from '../../widgets/forms/TinyDatePicker';
import TinyButton from '../../widgets/forms/TinyButton';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterDialog.scss';

const messages = defineMessages({
  filterBy: {
    id: 'wallet.transaction.filter.filterBy',
    defaultMessage: '!!!Filter by',
    description: 'Title of filter panel.',
  },
  reset: {
    id: 'wallet.transaction.filter.reset',
    defaultMessage: '!!!reset',
    description: 'Reset link text on filter panel.',
  },
  type: {
    id: 'wallet.transaction.filter.type',
    defaultMessage: '!!!Type',
    description: 'Filter Type.',
  },
  incoming: {
    id: 'wallet.transaction.filter.incoming',
    defaultMessage: '!!!Incoming',
    description: 'Incoming filter type.',
  },
  outgoing: {
    id: 'wallet.transaction.filter.outgoing',
    defaultMessage: '!!!Outgoing',
    description: 'Outgoing filter type.',
  },
  dateRange: {
    id: 'wallet.transaction.filter.dateRange',
    defaultMessage: '!!!Date range',
    description: 'Date range of filter.',
  },
  thisWeek: {
    id: 'wallet.transaction.filter.thisWeek',
    defaultMessage: '!!!This week',
    description: 'This week date range of filter.',
  },
  thisMonth: {
    id: 'wallet.transaction.filter.thisMonth',
    defaultMessage: '!!!This month',
    description: 'This month date range of filter.',
  },
  thisYear: {
    id: 'wallet.transaction.filter.thisYear',
    defaultMessage: '!!!This year',
    description: 'This year date range of filter.',
  },
  customDateRange: {
    id: 'wallet.transaction.filter.customDateRange',
    defaultMessage: '!!!Custom date range',
    description: 'Custom date range of filter.',
  },
  amountRange: {
    id: 'wallet.transaction.filter.amountRange',
    defaultMessage: '!!!Amount range',
    description: 'Amount range of filter.',
  },
  filter: {
    id: 'wallet.transaction.filter.filter',
    defaultMessage: '!!!Filter',
    description: 'Filter button label.',
  },
});

const MIN_AMOUNT = 0.000001;
const AMOUNT_RAW_LENGTH_LIMIT = 10;

const calculateDateRange = (
  dateRange: string,
  customDateRange: { customFromDate: string, customToDate: string }
) => {
  const { customFromDate, customToDate } = customDateRange;
  let fromDate = null;
  let toDate = null;

  if (dateRange === DateRangeTypes.ALL) {
    fromDate = '';
    toDate = '';
  } else if (dateRange === DateRangeTypes.CUSTOM_DATE_RANGE) {
    fromDate = customFromDate;
    toDate = customToDate;
  } else {
    if (dateRange === DateRangeTypes.THIS_WEEK) {
      fromDate = moment().startOf('week');
    } else if (dateRange === DateRangeTypes.THIS_MONTH) {
      fromDate = moment().startOf('month');
    } else if (dateRange === DateRangeTypes.THIS_YEAR) {
      fromDate = moment().startOf('year');
    } else {
      fromDate = moment();
    }
    fromDate = fromDate.format('YYYY-MM-DD');
    toDate = moment().format('YYYY-MM-DD');
  }

  return { fromDate, toDate };
};

const formatDateValue = (
  date: string,
  defaultDate: string,
  dateFormat: string
) => {
  if (!date) {
    const formattedDefaultDate = moment(defaultDate).format(dateFormat);

    return <span className="undefined">{formattedDefaultDate}</span>;
  }

  return moment(date).format(dateFormat);
};

const formatAmountValue = (amount: string, defaultAmount: string) => {
  let inputAmount = amount || defaultAmount;
  if (inputAmount === '.') {
    inputAmount = '0';
  } else if (inputAmount[0] === '.') {
    inputAmount = `0${inputAmount}`;
  } else if (inputAmount[inputAmount.length - 1] === '.') {
    inputAmount = `${inputAmount}0`;
  }

  const amountBigNumber = new BigNumber(inputAmount);
  const amountClassName = amount ? '' : 'undefined';
  const content =
    inputAmount.length > AMOUNT_RAW_LENGTH_LIMIT
      ? formattedWalletAmount(amountBigNumber, false, false)
      : amountBigNumber.toFormat();

  return <span className={amountClassName}>{content}</span>;
};

const validateForm = (values: {
  dateRange: DateRangeType,
  customFromDate: string,
  customToDate: string,
  fromAmount: string,
  toAmount: string,
}) => {
  const {
    dateRange,
    customFromDate,
    customToDate,
    fromAmount,
    toAmount,
  } = values;

  if (
    (dateRange === DateRangeTypes.CUSTOM_DATE_RANGE &&
      customFromDate &&
      customToDate &&
      moment(customFromDate).valueOf() > moment(customToDate).valueOf()) ||
    (fromAmount && toAmount && Number(fromAmount) > Number(toAmount))
  ) {
    return false;
  }

  return true;
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

  dateRangeOptions = [
    {
      label: this.context.intl.formatMessage(globalMessages.all),
      value: DateRangeTypes.ALL,
    },
    {
      label: this.context.intl.formatMessage(messages.thisWeek),
      value: DateRangeTypes.THIS_WEEK,
    },
    {
      label: this.context.intl.formatMessage(messages.thisMonth),
      value: DateRangeTypes.THIS_MONTH,
    },
    {
      label: this.context.intl.formatMessage(messages.thisYear),
      value: DateRangeTypes.THIS_YEAR,
    },
    {
      label: this.context.intl.formatMessage(messages.customDateRange),
      value: DateRangeTypes.CUSTOM_DATE_RANGE,
    },
  ];

  form = new ReactToolboxMobxForm({
    fields: {
      incomingChecked: {
        type: 'checkbox',
        label: this.context.intl.formatMessage(messages.incoming),
        value: this.props.populatedFilterOptions.incomingChecked,
      },
      outgoingChecked: {
        type: 'checkbox',
        label: this.context.intl.formatMessage(messages.outgoing),
        value: this.props.populatedFilterOptions.outgoingChecked,
      },
      dateRange: {
        label: this.context.intl.formatMessage(messages.dateRange),
        value: this.props.populatedFilterOptions.dateRange,
      },
      customFromDate: {
        label: '',
        value: this.props.populatedFilterOptions.fromDate,
      },
      customToDate: {
        label: '',
        value: this.props.populatedFilterOptions.toDate,
      },
      fromAmount: {
        type: 'number',
        label: '',
        value: this.props.populatedFilterOptions.fromAmount,
      },
      toAmount: {
        type: 'number',
        label: '',
        value: this.props.populatedFilterOptions.toAmount,
      },
    },
  });

  handleSelfRef = (selfRef: any) => {
    if (
      selfRef &&
      selfRef.parentElement &&
      selfRef.parentElement.parentElement
    ) {
      const {
        parentElement: modalElement,
      } = selfRef.parentElement.parentElement;
      const { parentElement: overlayElement } = modalElement || {};
      modalElement.style.backgroundColor =
        'var(--theme-transactions-filter-modal-bg-color)';
      modalElement.style.borderRadius = '4px';
      modalElement.style.minWidth = 'auto';
      modalElement.style.position = 'absolute';
      modalElement.style.right = '10px';
      modalElement.style.top = '190px';
      if (overlayElement) {
        overlayElement.style.backgroundColor = 'transparent';
      }
    }
  };

  resetForm = () => {
    const {
      dateRange,
      fromAmount,
      toAmount,
      incomingChecked,
      outgoingChecked,
    } = emptyTransactionFilterOptions;

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
        const {
          dateRange,
          customFromDate,
          customToDate,
          ...rest
        } = form.values();
        const dateRangePayload = calculateDateRange(dateRange, {
          customFromDate,
          customToDate,
        });

        onFilter({
          ...rest,
          ...dateRangePayload,
          dateRange,
        });
      },
      onError: () => null,
    });

  isValidDate = (date: Object) => {
    return date.isSameOrBefore(moment().endOf('day'));
  };

  renderTypeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const incomingCheckboxField = form.$('incomingChecked');
    const outgoingCheckboxField = form.$('outgoingChecked');

    return (
      <div className={styles.type}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.type)}</label>
        </div>
        <div className={styles.body}>
          <div className={styles.typeCheckbox}>
            <TinyCheckbox {...incomingCheckboxField.bind()} />
          </div>
          <div className={styles.typeCheckbox}>
            <TinyCheckbox {...outgoingCheckboxField.bind()} />
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

  renderCustomDateRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const {
      locale,
      dateFormat,
      defaultFilterOptions: { fromDate, toDate },
    } = this.props;
    const defaultFromDate = fromDate || '1970-01-01';
    const defaultToDate = toDate || moment().format('YYYY-MM-DD');
    const customFromDateField = form.$('customFromDate');
    const customToDateField = form.$('customToDate');
    const { customFromDate, customToDate } = form.values();
    const customFromDateInnerValue = formatDateValue(
      customFromDate,
      defaultFromDate,
      dateFormat
    );
    const customToDateInnerValue = formatDateValue(
      customToDate,
      defaultToDate,
      dateFormat
    );

    return (
      <div className={styles.customDateRange}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.customDateRange)}</label>
          <DialogCloseButton
            onClose={() => form.select('dateRange').set(DateRangeTypes.ALL)}
          />
        </div>
        <div className={styles.body}>
          <div className={styles.dateRangeInput}>
            <TinyDatePicker
              {...customFromDateField.bind()}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeFrom)}
              innerValue={customFromDateInnerValue}
              pickerPanelPosition="left"
              closeOnSelect
              onReset={() => form.select('customFromDate').set('')}
              isValidDate={this.isValidDate}
              locale={locale}
              dateFormat={dateFormat}
            />
          </div>
          <div className={styles.dateRangeInput}>
            <TinyDatePicker
              {...customToDateField.bind()}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeTo)}
              innerValue={customToDateInnerValue}
              pickerPanelPosition="right"
              closeOnSelect
              onReset={() => form.select('customToDate').set('')}
              isValidDate={this.isValidDate}
              locale={locale}
              dateFormat={dateFormat}
            />
          </div>
        </div>
      </div>
    );
  };

  onAmountFieldBlur = (selector: string) => {
    const { form } = this;
    const { fromAmount, toAmount } = form.values();

    if (selector === 'from' && fromAmount < MIN_AMOUNT) {
      form.select('fromAmount').set(MIN_AMOUNT.toString());
    } else if (selector === 'to' && toAmount < MIN_AMOUNT) {
      form.select('toAmount').set(MIN_AMOUNT.toString());
    }
  };

  renderAmountRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const {
      defaultFilterOptions: { fromAmount, toAmount },
    } = this.props;
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
              onBlur={(evt: Event<HTMLElement>) => {
                fromAmountFieldProps.onBlur(evt);
                this.onAmountFieldBlur('from');
              }}
              useReadMode
              notNegative
              maxLength={20}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeFrom)}
              innerLabelSuffix={intl.formatMessage(globalMessages.unitAda)}
              innerValue={fromAmountInnerValue}
            />
          </div>
          <div className={styles.amountRangeInput}>
            <TinyInput
              {...toAmountFieldProps}
              onBlur={(evt: Event<HTMLElement>) => {
                toAmountFieldProps.onBlur(evt);
                this.onAmountFieldBlur('to');
              }}
              useReadMode
              notNegative
              maxLength={20}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeTo)}
              innerLabelSuffix={intl.formatMessage(globalMessages.unitAda)}
              innerValue={toAmountInnerValue}
            />
          </div>
        </div>
      </div>
    );
  };

  renderActionButton = () => {
    const { intl } = this.context;
    const isFormValid = validateForm(this.form.values());

    return (
      <div className={styles.action}>
        <TinyButton
          label={intl.formatMessage(messages.filter)}
          loading={false}
          disabled={!isFormValid}
          onClick={this.handleSubmit}
        />
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { onClose } = this.props;
    const { dateRange } = this.form.values();
    const isCustomDateRangeSelected =
      dateRange === DateRangeTypes.CUSTOM_DATE_RANGE;

    return (
      <Dialog
        closeOnOverlayClick
        className={styles.component}
        onClose={onClose}
      >
        <div ref={this.handleSelfRef}>
          <div className={styles.title}>
            <h4 className={styles.titleText}>
              {intl.formatMessage(messages.filterBy)}
            </h4>
            <button className={styles.titleLink} onClick={this.resetForm}>
              {intl.formatMessage(messages.reset)}
            </button>
          </div>
          <div className={styles.content}>
            {this.renderTypeField()}
            {!isCustomDateRangeSelected && this.renderDateRangeField()}
            {isCustomDateRangeSelected && this.renderCustomDateRangeField()}
            {this.renderAmountRangeField()}
            {this.renderActionButton()}
          </div>
        </div>
      </Dialog>
    );
  }
}
