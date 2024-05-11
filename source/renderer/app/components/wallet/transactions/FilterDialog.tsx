// @ts-nocheck
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component, createRef } from 'react';
// @ts-ignore ts-migrate(2724) FIXME: '"react"' has no exported member named 'Element'. ... Remove this comment to see the full error message
import type { Element, ElementRef, Config } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { isEqual, pick } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from '@react-polymorph/components/PopOver';
import classNames from 'classnames';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import {
  DECIMAL_PLACES_IN_ADA,
  MIN_DISCREET_MODE_INPUT_FIELD_VALUE,
  MAX_DISCREET_MODE_INPUT_FIELD_VALUE,
} from '../../../config/numbersConfig';
import {
  calculateDateRange,
  validateFilterForm,
} from '../../../utils/transaction';
import type { TransactionFilterOptionsType } from '../../../stores/TransactionsStore';
import {
  DateRangeTypes,
  emptyTransactionFilterOptions,
} from '../../../stores/TransactionsStore';
import { withDiscreetMode } from '../../../features/discreet-mode';
import type { DiscreetModeFeature } from '../../../features/discreet-mode';
import { NUMBER_FORMATS } from '../../../../../common/types/number.types';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinySelect from '../../widgets/forms/TinySelect';
import TinyInput from '../../widgets/forms/TinyInput';
import TinyDatePicker from '../../widgets/forms/TinyDatePicker';
import TinyButton from '../../widgets/forms/TinyButton';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterDialog.scss';

const messages = defineMessages({
  allTransactions: {
    id: 'wallet.transaction.filter.allTransactions',
    defaultMessage: '!!!All Transactions',
    description: 'All Transactions button label.',
  },
  resetFilter: {
    id: 'wallet.transaction.filter.resetFilter',
    defaultMessage: '!!!Reset Filter',
    description: 'Reset Filter button label.',
  },
  incoming: {
    id: 'wallet.transaction.filter.incoming',
    defaultMessage: '!!!Received',
    description: 'Incoming filter type.',
  },
  outgoing: {
    id: 'wallet.transaction.filter.outgoing',
    defaultMessage: '!!!Sent',
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
type InjectedProps = {
  discreetModeFeature: DiscreetModeFeature;
};
export type FilterDialogProps = {
  locale: string;
  dateFormat: string;
  numberFormat: string;
  defaultFilterOptions: TransactionFilterOptionsType;
  populatedFilterOptions: TransactionFilterOptionsType;
  onFilter: (...args: Array<any>) => any;
  isDisabled: boolean;
  triggerElement?: Element<any>;
};
type Props = FilterDialogProps & InjectedProps;

interface FormFields {
  incomingChecked: boolean;
  outgoingChecked: boolean;
  dateRange: string;
  fromDate: string;
  toDate: string;
  fromAmount: string;
  toAmount: string;
}

class FilterDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  dateRangeOptions: Array<{
    label: string;
    value: string;
  }>;
  form: ReactToolboxMobxForm<FormFields>;
  popoverTippyInstance: ElementRef<any> = createRef();

  constructor(props: Props, context: Record<string, any>) {
    super(props);
    const {
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
    const { intl } = context;
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
    this.form = new ReactToolboxMobxForm<FormFields>({
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
  fillFormFields = (
    filterOptions: TransactionFilterOptionsType,
    reset?: boolean
  ) => {
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
    this.form
      .select('fromAmount')
      .set(reset ? fromAmount : this.getFromAmountValue(fromAmount));
    this.form
      .select('toAmount')
      .set(reset ? toAmount : this.getToAmountValue(toAmount));
    this.form.select('incomingChecked').set(incomingChecked);
    this.form.select('outgoingChecked').set(outgoingChecked);
  };
  getFromAmountValue = (fromAmount?: string) => {
    const { discreetModeFeature } = this.props;
    return discreetModeFeature.isDiscreetMode
      ? MIN_DISCREET_MODE_INPUT_FIELD_VALUE.toString()
      : fromAmount;
  };
  getToAmountValue = (toAmount?: string) => {
    const { discreetModeFeature } = this.props;
    return discreetModeFeature.isDiscreetMode
      ? MAX_DISCREET_MODE_INPUT_FIELD_VALUE.toString()
      : toAmount;
  };
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ searchTerm: string; searchLimi... Remove this comment to see the full error message
  resetForm = () => this.fillFormFields(emptyTransactionFilterOptions, true);
  getDefaultFilterOptions = (): TransactionFilterOptionsType => {
    const { defaultFilterOptions } = this.props;
    return {
      ...defaultFilterOptions,
      fromAmount: this.getFromAmountValue(defaultFilterOptions.fromAmount),
      toAmount: this.getToAmountValue(defaultFilterOptions.toAmount),
    };
  };
  generateDefaultFilterOptions = () =>
    this.fillFormFields(this.getDefaultFilterOptions());
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
  handleSubmit = () => {
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

    // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
    if (this.popoverTippyInstance.current) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
      this.popoverTippyInstance.current.hide();
    }
  };
  isValidFromDate = (date: Record<string, any>) => {
    return date.isSameOrBefore(moment().endOf('day'));
  };
  isValidToDate = (date: Record<string, any>) => {
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
              onChange={(isSelected) =>
                this.setFilterType('incomingChecked', isSelected)
              }
            />
          </div>
          <div className={styles.typeCheckbox}>
            <TinyCheckbox
              {...outgoingCheckboxField.bind()}
              onChange={(isSelected) =>
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
      styles.fromDateInput,
      fromDateLocaleClassName,
    ]);
    const toDateClassNames = classNames([
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
              value={fromAmountField.value}
              onSubmit={this.handleSubmit}
              label={intl.formatMessage(globalMessages.rangeFrom)}
              bigNumberFormat={NUMBER_FORMATS[numberFormat]}
              decimalPlaces={DECIMAL_PLACES_IN_ADA}
              allowSigns={false}
            />
          </div>
          <div className={toAmountClassNames}>
            <TinyInput
              {...toAmountField.bind()}
              value={toAmountField.value}
              onSubmit={this.handleSubmit}
              label={intl.formatMessage(globalMessages.rangeTo)}
              bigNumberFormat={NUMBER_FORMATS[numberFormat]}
              decimalPlaces={DECIMAL_PLACES_IN_ADA}
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
    const { triggerElement, isDisabled } = this.props;
    return (
      <PopOver
        arrow={false}
        interactive
        disabled={isDisabled}
        trigger="click"
        appendTo={document.body}
        onShow={(instance) => {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
          this.popoverTippyInstance.current = instance;
        }}
        duration={0}
        offset={[0, 10]}
        maxWidth={640}
        placement="bottom"
        themeVariables={{
          '--rp-pop-over-bg-color':
            'var(--theme-transactions-filter-modal-bg-color)',
          '--rp-pop-over-box-shadow': '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
          '--rp-pop-over-border-radius': '4px',
          '--rp-pop-over-border-style': 'solid',
          '--rp-pop-over-padding': 0,
        }}
        popperOptions={{
          modifiers: [
            {
              // This keeps the popover always 20px away from the screen edge
              name: 'preventOverflow',
              options: {
                padding: 20,
              },
            },
          ],
        }}
        content={
          <div className={styles.component}>
            <div className={styles.title}>
              <h4 className={styles.titleText}>
                {intl.formatMessage(globalMessages.filter)}
              </h4>
              <div>
                <button
                  className={styles.titleLink}
                  onClick={this.generateDefaultFilterOptions}
                  disabled={this.isFormValuesEqualTo(
                    this.getDefaultFilterOptions()
                  )}
                >
                  {intl.formatMessage(messages.allTransactions)}
                </button>
                <button
                  className={styles.titleLink}
                  onClick={this.resetForm}
                  disabled={this.isFormValuesEqualTo(
                    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ searchTerm: string; searchLimi... Remove this comment to see the full error message
                    emptyTransactionFilterOptions
                  )}
                >
                  {intl.formatMessage(messages.resetFilter)}
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
        }
      >
        {triggerElement}
      </PopOver>
    );
  }
}

export default withDiscreetMode<Config<Props, InjectedProps>>(
  observer(FilterDialog)
);
