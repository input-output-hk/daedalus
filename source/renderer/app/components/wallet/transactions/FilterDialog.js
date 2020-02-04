// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import type { ElementRef } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { i18nContext } from '../../../utils/i18nContext';
import {
  calculateDateRange,
  formatDateValue,
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
        label: intl.formatMessage(globalMessages.all),
        value: DateRangeTypes.ALL,
      },
      {
        label: intl.formatMessage(messages.thisWeek),
        value: DateRangeTypes.THIS_WEEK,
      },
      {
        label: intl.formatMessage(messages.thisMonth),
        value: DateRangeTypes.THIS_MONTH,
      },
      {
        label: intl.formatMessage(messages.thisYear),
        value: DateRangeTypes.THIS_YEAR,
      },
      {
        label: intl.formatMessage(messages.customDateRange),
        value: DateRangeTypes.CUSTOM_DATE_RANGE,
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
        customFromDate: {
          label: '',
          value: fromDate,
        },
        customToDate: {
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

    if (this.form.select('customFromDate')) {
      this.form.select('customFromDate').set(fromDate);
    }
    if (this.form.select('customToDate')) {
      this.form.select('customToDate').set(toDate);
    }
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
        if (validateFilterForm(form.values())) {
          const dateRangePayload = calculateDateRange(dateRange, {
            customFromDate,
            customToDate,
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

    if (selector === 'from' && fromAmount) {
      if (Number.isNaN(Number(fromAmount))) {
        form.select('fromAmount').set('');
      } else {
        const fromValue = new BigNumber(fromAmount).toString();
        form.select('fromAmount').set(fromValue);
      }
    } else if (selector === 'to' && toAmount) {
      if (Number.isNaN(Number(toAmount))) {
        form.select('toAmount').set('');
      } else {
        const toValue = new BigNumber(toAmount).toString();
        form.select('toAmount').set(toValue);
      }
    }
  };

  onAmountFieldChange = (
    fieldName: 'fromAmount' | 'toAmount',
    newValue: string
  ) => {
    const field = this.form.select(fieldName);
    const oldValue = field.value;
    let value = newValue;
    if (newValue === '0') value = oldValue === '0.' ? '' : '0.';
    field.set(value);
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
              onBlur={(evt: SyntheticEvent<EventTarget>) => {
                fromAmountFieldProps.onBlur(evt);
                this.onAmountFieldBlur('from');
              }}
              onChange={(value: string) =>
                this.onAmountFieldChange('fromAmount', value)
              }
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
                this.onAmountFieldBlur('to');
              }}
              onChange={(value: string) =>
                this.onAmountFieldChange('toAmount', value)
              }
              onSubmit={this.handleSubmit}
              useReadMode
              notNegative
              digitCountAfterDecimalPoint={6}
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
    const isFormValid = validateFilterForm(this.form.values());

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
