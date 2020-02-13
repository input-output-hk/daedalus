// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import type { ElementRef } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { isEqual, pick } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { i18nContext } from '../../../utils/i18nContext';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import {
  calculateDateRange,
  validateFilterForm,
} from '../../../utils/transaction';
import type { TransactionFilterOptionsType } from '../../../stores/TransactionsStore';
import {
  DateRangeTypes,
  emptyTransactionFilterOptions,
} from '../../../stores/TransactionsStore';
import { NUMBER_FORMATS } from '../../../../../common/types/number.types';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinySelect from '../../widgets/forms/TinySelect';
import TinyInput from '../../widgets/forms/TinyInput';
import TinyDatePicker from '../../widgets/forms/TinyDatePicker';
import TinyButton from '../../widgets/forms/TinyButton';
import Dialog from '../../widgets/Dialog';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterDialog.scss';

const messages = defineMessages({
  allTransactions: {
    id: 'wallet.transaction.filter.allTransactions',
    defaultMessage: '!!!All Transactions',
    description: 'All Transactions button label.',
  },
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
  selectTimeRange: {
    id: 'wallet.transaction.filter.selectTimeRange',
    defaultMessage: '!!!Select time range',
    description: 'Select time range indication of filter.',
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
const FILTER_DIALOG_WITH_DATE_PICKER_HEIGHT = 545;

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
  numberFormat: string,
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
          value: fromAmount ? Number(fromAmount) : '',
        },
        toAmount: {
          type: 'number',
          label: '',
          value: toAmount ? Number(toAmount) : '',
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

  fillFormFields = (filterOptions: TransactionFilterOptionsType) => {
    const {
      dateRange,
      fromDate,
      toDate,
      fromAmount,
      toAmount,
      incomingChecked,
      outgoingChecked,
    } = filterOptions;

    this.form.select('dateRange').set(dateRange);
    this.form.select('fromDate').set(fromDate);
    this.form.select('toDate').set(toDate);
    this.form.select('fromAmount').set(fromAmount ? Number(fromAmount) : '');
    this.form.select('toAmount').set(toAmount ? Number(toAmount) : '');
    this.form.select('incomingChecked').set(incomingChecked);
    this.form.select('outgoingChecked').set(outgoingChecked);
  };

  resetForm = () => this.fillFormFields(emptyTransactionFilterOptions);

  generateDefaultFilterOptions = () =>
    this.fillFormFields(this.props.defaultFilterOptions);

  isFormValuesEqualTo = (
    comparedFilterOptions: TransactionFilterOptionsType
  ) => {
    const formFieldNames = Object.keys(this.form.fields.toJSON());
    return isEqual(
      this.getComposedFormValues(),
      pick(comparedFilterOptions, formFieldNames)
    );
  };

  getComposedFormValues = () => {
    const formValues = this.form.values();
    return {
      ...formValues,
      fromAmount: formValues.fromAmount.toString(),
      toAmount: formValues.toAmount.toString(),
    };
  };

  handleSubmit = () =>
    this.form.submit({
      onSuccess: () => {
        const { onFilter } = this.props;
        const formValues = this.getComposedFormValues();
        if (validateFilterForm(formValues).isValid) {
          onFilter(formValues);
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
    const { intl } = this.context;
    const dateRangeFieldBindProps = this.form.$('dateRange').bind();
    const { fromDate, toDate } = this.form.values();

    return (
      <div className={styles.dateRange}>
        <TinySelect
          {...dateRangeFieldBindProps}
          onChange={(...args) => {
            dateRangeFieldBindProps.onChange(...args);
            const calculatedDateRange = calculateDateRange(args[0], {
              fromDate,
              toDate,
            });
            this.form.select('fromDate').set(calculatedDateRange.fromDate);
            this.form.select('toDate').set(calculatedDateRange.toDate);
          }}
          placeholder={intl.formatMessage(messages.selectTimeRange)}
          options={this.dateRangeOptions}
        />
      </div>
    );
  };

  renderDateRangeFromToField = () => {
    const { form } = this;
    const { intl } = this.context;
    const { locale, dateFormat } = this.props;
    const { invalidFields } = validateFilterForm(this.getComposedFormValues());
    const fromDateFieldBindProps = form.$('fromDate').bind();
    const toDateFieldBindProps = form.$('toDate').bind();
    const fromDateLocaleClassName =
      locale === 'ja-JP' ? styles.japaneseFromDateInput : null;
    const toDateLocaleClassName =
      locale === 'ja-JP' ? styles.japaneseToDateInput : null;
    const fromDateClassNames = classNames([
      styles.dateRangeInput,
      styles.fromDateInput,
      fromDateLocaleClassName,
    ]);
    const toDateClassNames = classNames([
      styles.dateRangeInput,
      styles.toDateInput,
      toDateLocaleClassName,
    ]);

    return (
      <div className={styles.dateRangeFromTo}>
        <div className={styles.body}>
          <div className={fromDateClassNames}>
            <TinyDatePicker
              {...fromDateFieldBindProps}
              onChange={(...args) => {
                fromDateFieldBindProps.onChange(...args);
                this.form.select('dateRange').set(DateRangeTypes.CUSTOM);
              }}
              label={intl.formatMessage(globalMessages.rangeFrom)}
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
              {...toDateFieldBindProps}
              onChange={(...args) => {
                toDateFieldBindProps.onChange(...args);
                this.form.select('dateRange').set(DateRangeTypes.CUSTOM);
              }}
              label={intl.formatMessage(globalMessages.rangeTo)}
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

  renderAmountRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const { locale, numberFormat } = this.props;
    const { invalidFields } = validateFilterForm(this.getComposedFormValues());
    const fromAmountField = form.$('fromAmount');
    const toAmountField = form.$('toAmount');
    const fromAmountLocaleClassName =
      locale === 'ja-JP' ? styles.japaneseFromAmountInput : null;
    const toAmountLocaleClassName =
      locale === 'ja-JP' ? styles.japaneseToAmountInput : null;
    const fromAmountClassNames = classNames([
      styles.amountRangeInput,
      styles.fromAmountInput,
      fromAmountLocaleClassName,
    ]);
    const toAmountClassNames = classNames([
      styles.amountRangeInput,
      styles.toAmountInput,
      toAmountLocaleClassName,
    ]);

    return (
      <div className={styles.amountRange}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.amountRange)}</label>
        </div>
        <div className={styles.body}>
          <div className={fromAmountClassNames}>
            <TinyInput
              {...fromAmountField.bind()}
              onSubmit={this.handleSubmit}
              label={intl.formatMessage(globalMessages.rangeFrom)}
              numberFormat={NUMBER_FORMATS[numberFormat]}
              numberLocaleOptions={{
                minimumFractionDigits: DECIMAL_PLACES_IN_ADA,
              }}
              allowSigns={false}
            />
          </div>
          <div className={toAmountClassNames}>
            <TinyInput
              {...toAmountField.bind()}
              onSubmit={this.handleSubmit}
              label={intl.formatMessage(globalMessages.rangeTo)}
              numberFormat={NUMBER_FORMATS[numberFormat]}
              numberLocaleOptions={{
                minimumFractionDigits: DECIMAL_PLACES_IN_ADA,
              }}
              allowSigns={false}
              error={invalidFields.toAmount}
            />
          </div>
        </div>
      </div>
    );
  };

  renderActionButton = () => {
    const { intl } = this.context;
    const { populatedFilterOptions } = this.props;
    const { isValid } = validateFilterForm(this.getComposedFormValues());

    return (
      <div className={styles.action}>
        <TinyButton
          label={intl.formatMessage(messages.apply)}
          loading={false}
          disabled={
            this.isFormValuesEqualTo(populatedFilterOptions) || !isValid
          }
          onClick={this.handleSubmit}
        />
      </div>
    );
  };

  render() {
    const { intl } = this.context;
    const { defaultFilterOptions, onClose } = this.props;

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
            <div>
              <button
                className={styles.titleLink}
                onClick={this.generateDefaultFilterOptions}
                disabled={this.isFormValuesEqualTo(defaultFilterOptions)}
              >
                {intl.formatMessage(messages.allTransactions)}
              </button>
              <button
                className={styles.titleLink}
                onClick={this.resetForm}
                disabled={this.isFormValuesEqualTo(
                  emptyTransactionFilterOptions
                )}
              >
                {intl.formatMessage(globalMessages.reset)}
              </button>
            </div>
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
