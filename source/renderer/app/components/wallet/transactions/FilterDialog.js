'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
// @ts-nocheck
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const moment_1 = __importDefault(require('moment'));
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const classnames_1 = __importDefault(require('classnames'));
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const numbersConfig_1 = require('../../../config/numbersConfig');
const transaction_1 = require('../../../utils/transaction');
const TransactionsStore_1 = require('../../../stores/TransactionsStore');
const discreet_mode_1 = require('../../../features/discreet-mode');
const number_types_1 = require('../../../../../common/types/number.types');
const TinyCheckbox_1 = __importDefault(
  require('../../widgets/forms/TinyCheckbox')
);
const TinySelect_1 = __importDefault(require('../../widgets/forms/TinySelect'));
const TinyInput_1 = __importDefault(require('../../widgets/forms/TinyInput'));
const TinyDatePicker_1 = __importDefault(
  require('../../widgets/forms/TinyDatePicker')
);
const TinyButton_1 = __importDefault(require('../../widgets/forms/TinyButton'));
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const FilterDialog_scss_1 = __importDefault(require('./FilterDialog.scss'));
const messages = (0, react_intl_1.defineMessages)({
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
let FilterDialog = class FilterDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  dateRangeOptions;
  form;
  popoverTippyInstance = (0, react_1.createRef)();
  constructor(props, context) {
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
        value: TransactionsStore_1.DateRangeTypes.LAST_7_DAYS,
      },
      {
        label: intl.formatMessage(messages.last30Days),
        value: TransactionsStore_1.DateRangeTypes.LAST_30_DAYS,
      },
      {
        label: intl.formatMessage(messages.last90Days),
        value: TransactionsStore_1.DateRangeTypes.LAST_90_DAYS,
      },
      {
        label: intl.formatMessage(messages.thisYear),
        value: TransactionsStore_1.DateRangeTypes.THIS_YEAR,
      },
      {
        label: intl.formatMessage(messages.custom),
        value: TransactionsStore_1.DateRangeTypes.CUSTOM,
      },
    ];
    this.form = new ReactToolboxMobxForm_1.default({
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
  setFilterType = (field, value) => {
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
  fillFormFields = (filterOptions, reset) => {
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
  getFromAmountValue = (fromAmount) => {
    const { discreetModeFeature } = this.props;
    return discreetModeFeature.isDiscreetMode
      ? numbersConfig_1.MIN_DISCREET_MODE_INPUT_FIELD_VALUE.toString()
      : fromAmount;
  };
  getToAmountValue = (toAmount) => {
    const { discreetModeFeature } = this.props;
    return discreetModeFeature.isDiscreetMode
      ? numbersConfig_1.MAX_DISCREET_MODE_INPUT_FIELD_VALUE.toString()
      : toAmount;
  };
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ searchTerm: string; searchLimi... Remove this comment to see the full error message
  resetForm = () =>
    this.fillFormFields(
      TransactionsStore_1.emptyTransactionFilterOptions,
      true
    );
  getDefaultFilterOptions = () => {
    const { defaultFilterOptions } = this.props;
    return {
      ...defaultFilterOptions,
      fromAmount: this.getFromAmountValue(defaultFilterOptions.fromAmount),
      toAmount: this.getToAmountValue(defaultFilterOptions.toAmount),
    };
  };
  generateDefaultFilterOptions = () =>
    this.fillFormFields(this.getDefaultFilterOptions());
  isFormValuesEqualTo = (comparedFilterOptions) => {
    const formFieldNames = Object.keys(this.form.fields.toJSON());
    return (0, lodash_1.isEqual)(
      this.getComposedFormValues(),
      (0, lodash_1.pick)(comparedFilterOptions, formFieldNames)
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
        if ((0, transaction_1.validateFilterForm)(formValues).isValid) {
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
  isValidFromDate = (date) => {
    return date.isSameOrBefore((0, moment_1.default)().endOf('day'));
  };
  isValidToDate = (date) => {
    const { fromDate } = this.form.values();
    return (
      date.isSameOrBefore((0, moment_1.default)().endOf('day')) &&
      date.isSameOrAfter((0, moment_1.default)(fromDate).startOf('day'))
    );
  };
  renderTypeField = () => {
    const { form } = this;
    const incomingCheckboxField = form.$('incomingChecked');
    const outgoingCheckboxField = form.$('outgoingChecked');
    return react_1.default.createElement(
      'div',
      { className: FilterDialog_scss_1.default.type },
      react_1.default.createElement(
        'div',
        { className: FilterDialog_scss_1.default.body },
        react_1.default.createElement(
          'div',
          { className: FilterDialog_scss_1.default.typeCheckbox },
          react_1.default.createElement(TinyCheckbox_1.default, {
            ...incomingCheckboxField.bind(),
            onChange: (isSelected) =>
              this.setFilterType('incomingChecked', isSelected),
          })
        ),
        react_1.default.createElement(
          'div',
          { className: FilterDialog_scss_1.default.typeCheckbox },
          react_1.default.createElement(TinyCheckbox_1.default, {
            ...outgoingCheckboxField.bind(),
            onChange: (isSelected) =>
              this.setFilterType('outgoingChecked', isSelected),
          })
        )
      )
    );
  };
  renderDateRangeField = () => {
    const { intl } = this.context;
    const dateRangeFieldBindProps = this.form.$('dateRange').bind();
    const { fromDate, toDate } = this.form.values();
    return react_1.default.createElement(
      'div',
      { className: FilterDialog_scss_1.default.dateRange },
      react_1.default.createElement(TinySelect_1.default, {
        ...dateRangeFieldBindProps,
        onChange: (...args) => {
          dateRangeFieldBindProps.onChange(...args);
          const calculatedDateRange = (0, transaction_1.calculateDateRange)(
            args[0],
            {
              fromDate,
              toDate,
            }
          );
          this.form.select('fromDate').set(calculatedDateRange.fromDate);
          this.form.select('toDate').set(calculatedDateRange.toDate);
        },
        placeholder: intl.formatMessage(messages.selectTimeRange),
        options: this.dateRangeOptions,
      })
    );
  };
  renderDateRangeFromToField = () => {
    const { form } = this;
    const { intl } = this.context;
    const { locale, dateFormat } = this.props;
    const { invalidFields } = (0, transaction_1.validateFilterForm)(
      this.getComposedFormValues()
    );
    const fromDateFieldBindProps = form.$('fromDate').bind();
    const toDateFieldBindProps = form.$('toDate').bind();
    const fromDateLocaleClassName =
      locale === 'ja-JP'
        ? FilterDialog_scss_1.default.japaneseFromDateInput
        : null;
    const toDateLocaleClassName =
      locale === 'ja-JP'
        ? FilterDialog_scss_1.default.japaneseToDateInput
        : null;
    const fromDateClassNames = (0, classnames_1.default)([
      FilterDialog_scss_1.default.fromDateInput,
      fromDateLocaleClassName,
    ]);
    const toDateClassNames = (0, classnames_1.default)([
      FilterDialog_scss_1.default.toDateInput,
      toDateLocaleClassName,
    ]);
    return react_1.default.createElement(
      'div',
      { className: FilterDialog_scss_1.default.dateRangeFromTo },
      react_1.default.createElement(
        'div',
        { className: FilterDialog_scss_1.default.body },
        react_1.default.createElement(
          'div',
          { className: fromDateClassNames },
          react_1.default.createElement(TinyDatePicker_1.default, {
            ...fromDateFieldBindProps,
            onChange: (...args) => {
              fromDateFieldBindProps.onChange(...args);
              this.form
                .select('dateRange')
                .set(TransactionsStore_1.DateRangeTypes.CUSTOM);
            },
            label: intl.formatMessage(global_messages_1.default.rangeFrom),
            pickerPanelPosition: 'left',
            closeOnSelect: true,
            onReset: () => form.select('fromDate').set(''),
            isValidDate: this.isValidFromDate,
            locale: locale,
            dateFormat: dateFormat,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: toDateClassNames },
          react_1.default.createElement(TinyDatePicker_1.default, {
            ...toDateFieldBindProps,
            onChange: (...args) => {
              toDateFieldBindProps.onChange(...args);
              this.form
                .select('dateRange')
                .set(TransactionsStore_1.DateRangeTypes.CUSTOM);
            },
            label: intl.formatMessage(global_messages_1.default.rangeTo),
            pickerPanelPosition: 'right',
            closeOnSelect: true,
            onReset: () => form.select('toDate').set(''),
            isValidDate: this.isValidToDate,
            locale: locale,
            dateFormat: dateFormat,
            error: invalidFields.toDate,
          })
        )
      )
    );
  };
  renderAmountRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const { locale, numberFormat } = this.props;
    const { invalidFields } = (0, transaction_1.validateFilterForm)(
      this.getComposedFormValues()
    );
    const fromAmountField = form.$('fromAmount');
    const toAmountField = form.$('toAmount');
    const fromAmountLocaleClassName =
      locale === 'ja-JP'
        ? FilterDialog_scss_1.default.japaneseFromAmountInput
        : null;
    const toAmountLocaleClassName =
      locale === 'ja-JP'
        ? FilterDialog_scss_1.default.japaneseToAmountInput
        : null;
    const fromAmountClassNames = (0, classnames_1.default)([
      FilterDialog_scss_1.default.amountRangeInput,
      FilterDialog_scss_1.default.fromAmountInput,
      fromAmountLocaleClassName,
    ]);
    const toAmountClassNames = (0, classnames_1.default)([
      FilterDialog_scss_1.default.amountRangeInput,
      FilterDialog_scss_1.default.toAmountInput,
      toAmountLocaleClassName,
    ]);
    return react_1.default.createElement(
      'div',
      { className: FilterDialog_scss_1.default.amountRange },
      react_1.default.createElement(
        'div',
        { className: FilterDialog_scss_1.default.header },
        react_1.default.createElement(
          'label',
          null,
          intl.formatMessage(messages.amountRange)
        )
      ),
      react_1.default.createElement(
        'div',
        { className: FilterDialog_scss_1.default.body },
        react_1.default.createElement(
          'div',
          { className: fromAmountClassNames },
          react_1.default.createElement(TinyInput_1.default, {
            ...fromAmountField.bind(),
            value: fromAmountField.value,
            onSubmit: this.handleSubmit,
            label: intl.formatMessage(global_messages_1.default.rangeFrom),
            bigNumberFormat: number_types_1.NUMBER_FORMATS[numberFormat],
            decimalPlaces: numbersConfig_1.DECIMAL_PLACES_IN_ADA,
            allowSigns: false,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: toAmountClassNames },
          react_1.default.createElement(TinyInput_1.default, {
            ...toAmountField.bind(),
            value: toAmountField.value,
            onSubmit: this.handleSubmit,
            label: intl.formatMessage(global_messages_1.default.rangeTo),
            bigNumberFormat: number_types_1.NUMBER_FORMATS[numberFormat],
            decimalPlaces: numbersConfig_1.DECIMAL_PLACES_IN_ADA,
            allowSigns: false,
            error: invalidFields.toAmount,
          })
        )
      )
    );
  };
  renderActionButton = () => {
    const { intl } = this.context;
    const { populatedFilterOptions } = this.props;
    const { isValid } = (0, transaction_1.validateFilterForm)(
      this.getComposedFormValues()
    );
    return react_1.default.createElement(
      'div',
      { className: FilterDialog_scss_1.default.action },
      react_1.default.createElement(TinyButton_1.default, {
        label: intl.formatMessage(messages.apply),
        loading: false,
        disabled: this.isFormValuesEqualTo(populatedFilterOptions) || !isValid,
        onClick: this.handleSubmit,
      })
    );
  };
  render() {
    const { intl } = this.context;
    const { triggerElement, isDisabled } = this.props;
    return react_1.default.createElement(
      PopOver_1.PopOver,
      {
        arrow: false,
        interactive: true,
        disabled: isDisabled,
        trigger: 'click',
        appendTo: document.body,
        onShow: (instance) => {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'current' does not exist on type 'unknown... Remove this comment to see the full error message
          this.popoverTippyInstance.current = instance;
        },
        duration: 0,
        offset: [0, 10],
        maxWidth: 640,
        placement: 'bottom',
        themeVariables: {
          '--rp-pop-over-bg-color':
            'var(--theme-transactions-filter-modal-bg-color)',
          '--rp-pop-over-box-shadow': '0 5px 20px 0 rgba(0, 0, 0, 0.25)',
          '--rp-pop-over-border-radius': '4px',
          '--rp-pop-over-border-style': 'solid',
          '--rp-pop-over-padding': 0,
        },
        popperOptions: {
          modifiers: [
            {
              // This keeps the popover always 20px away from the screen edge
              name: 'preventOverflow',
              options: {
                padding: 20,
              },
            },
          ],
        },
        content: react_1.default.createElement(
          'div',
          { className: FilterDialog_scss_1.default.component },
          react_1.default.createElement(
            'div',
            { className: FilterDialog_scss_1.default.title },
            react_1.default.createElement(
              'h4',
              { className: FilterDialog_scss_1.default.titleText },
              intl.formatMessage(global_messages_1.default.filter)
            ),
            react_1.default.createElement(
              'div',
              null,
              react_1.default.createElement(
                'button',
                {
                  className: FilterDialog_scss_1.default.titleLink,
                  onClick: this.generateDefaultFilterOptions,
                  disabled: this.isFormValuesEqualTo(
                    this.getDefaultFilterOptions()
                  ),
                },
                intl.formatMessage(messages.allTransactions)
              ),
              react_1.default.createElement(
                'button',
                {
                  className: FilterDialog_scss_1.default.titleLink,
                  onClick: this.resetForm,
                  disabled: this.isFormValuesEqualTo(
                    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ searchTerm: string; searchLimi... Remove this comment to see the full error message
                    TransactionsStore_1.emptyTransactionFilterOptions
                  ),
                },
                intl.formatMessage(messages.resetFilter)
              )
            )
          ),
          react_1.default.createElement(
            'div',
            { className: FilterDialog_scss_1.default.content },
            this.renderTypeField(),
            this.renderDateRangeField(),
            this.renderDateRangeFromToField(),
            this.renderAmountRangeField(),
            this.renderActionButton()
          )
        ),
      },
      triggerElement
    );
  }
};
FilterDialog = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object, Object])],
  FilterDialog
);
exports.default = (0, discreet_mode_1.withDiscreetMode)(FilterDialog);
//# sourceMappingURL=FilterDialog.js.map
