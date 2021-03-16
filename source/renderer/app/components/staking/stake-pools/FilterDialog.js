// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component, createRef } from 'react';
import type { Element, ElementRef } from 'react';
import { observer } from 'mobx-react';
import { isEqual, pick } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import type { StakePoolFilterOptionsType } from '../../../stores/StakingStore';
import { emptyStakePoolFilterOptions } from '../../../stores/StakingStore';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinyButton from '../../widgets/forms/TinyButton';
import globalMessages from '../../../i18n/global-messages';
import styles from './FilterDialog.scss';

const messages = defineMessages({
  resetFilter: {
    id: 'staking.stakePools.filter.resetFilter',
    defaultMessage: '!!!Reset Filter',
    description: 'Reset Filter button label.',
  },
  retiringPools: {
    id: 'staking.stakePools.filter.retiringPools',
    defaultMessage: '!!!Retiring pools',
    description: 'Retiring pools filter type.',
  },
  privatePools: {
    id: 'staking.stakePools.filter.privatePools',
    defaultMessage: '!!!Private pools',
    description: 'Private pools filter type.',
  },
  poolsWithoutOffChainData: {
    id: 'staking.stakePools.filter.poolsWithoutOffChainData',
    defaultMessage: '!!!Pools without off-chain data',
    description: 'Pools without off-chain data filter type.',
  },
  apply: {
    id: 'staking.stakePools.filter.apply',
    defaultMessage: '!!!Apply',
    description: 'Filter button label.',
  },
});

export type FilterDialogProps = {
  locale: string,
  dateFormat: string,
  numberFormat: string,
  defaultFilterOptions: TransactionFilterOptionsType,
  populatedFilterOptions: TransactionFilterOptionsType,
  onFilter: Function,
  triggerElement?: Element<*>,
};

@observer
export default class FilterDialog extends Component<FilterDialogProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  dateRangeOptions: Array<{ label: string, value: string }>;
  form: ReactToolboxMobxForm;
  popoverTippyInstance: ElementRef<*> = createRef();

  constructor(props: FilterDialogProps, context: Object) {
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
    this.form.select('fromAmount').set(fromAmount);
    this.form.select('toAmount').set(toAmount);
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
    if (this.popoverTippyInstance.current) {
      this.popoverTippyInstance.current.hide();
    }
  };

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
    const { defaultFilterOptions, triggerElement } = this.props;

    return (
      <PopOver
        arrow={false}
        interactive
        trigger="click"
        appendTo={document.body}
        onShow={(instance) => {
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
