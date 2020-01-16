// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape } from 'react-intl';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import type { DateRangeType } from '../../../stores/TransactionsStore';
import { DateRangeTypes } from '../../../stores/TransactionsStore';
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
  amountUnit: {
    id: 'global.unit.ada',
    defaultMessage: '!!!ADA',
    description: 'Amount unit.',
  },
  filter: {
    id: 'wallet.transaction.filter.filter',
    defaultMessage: '!!!Filter',
    description: 'Filter button label.',
  },
});

const calculateDateRange = (
  dateRange: string,
  customDateRange: { customFromDate: string, customToDate: string },
  defaultDateRange: { defaultFromDate: string, defaultToDate: string }
) => {
  const { customFromDate, customToDate } = customDateRange;
  const { defaultFromDate = '', defaultToDate = '' } = defaultDateRange || {};
  let fromDate = null;
  let toDate = null;

  if (dateRange === DateRangeTypes.ALL) {
    fromDate = defaultFromDate;
    toDate = defaultToDate;
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

const formatDateValue = (date: string, defaultDate: string) => {
  if (!date) {
    const formattedDefaultDate = moment(defaultDate).format('DD.MM.YYYY');

    return <span className="undefined">{formattedDefaultDate}</span>;
  }

  return moment(date).format('DD.MM.YYYY');
};

const formatAmountValue = (amount: string, defaultAmount: string) => {
  if (!amount) {
    const formattedDefaultAmount = formattedWalletAmount(
      new BigNumber(defaultAmount),
      false,
      false
    );

    return <span className="undefined">{formattedDefaultAmount}</span>;
  }

  const amountBigNumber = new BigNumber(amount);

  if (amount.length > 5) {
    return formattedWalletAmount(amountBigNumber, false, false);
  }

  return amountBigNumber.toFormat();
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
  dateRange?: DateRangeType,
  fromDate?: string,
  toDate?: string,
  fromAmount?: string,
  toAmount?: string,
  incomingChecked?: boolean,
  outgoingChecked?: boolean,
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
        value: this.props.incomingChecked,
      },
      outgoingChecked: {
        type: 'checkbox',
        label: this.context.intl.formatMessage(messages.outgoing),
        value: this.props.outgoingChecked,
      },
      dateRange: {
        label: this.context.intl.formatMessage(messages.dateRange),
        value: this.props.dateRange,
      },
      customFromDate: {
        label: '',
        value: this.props.fromDate,
      },
      customToDate: {
        label: '',
        value: this.props.toDate,
      },
      fromAmount: {
        type: 'number',
        label: '',
        value: this.props.fromAmount,
      },
      toAmount: {
        type: 'number',
        label: '',
        value: this.props.toAmount,
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
    this.form.select('dateRange').set(DateRangeTypes.ALL);
    this.form.select('fromAmount').set(this.props.fromAmount || '');
    this.form.select('toAmount').set(this.props.toAmount || '');
    this.form.select('incomingChecked').set(true);
    this.form.select('outgoingChecked').set(true);
  };

  handleSubmit = () =>
    this.form.submit({
      onSuccess: form => {
        const { onFilter, fromDate, toDate } = this.props;
        const {
          dateRange,
          customFromDate,
          customToDate,
          ...rest
        } = form.values();
        const dateRangePayload = calculateDateRange(
          dateRange,
          { customFromDate, customToDate },
          { defaultFromDate: fromDate || '', defaultToDate: toDate || '' }
        );

        onFilter({
          ...rest,
          ...dateRangePayload,
          dateRange,
        });
      },
      onError: () => null,
    });

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
    const { fromDate, toDate } = this.props;
    const defaultFromDate = fromDate || '1970-01-01';
    const defaultToDate = toDate || moment().format('YYYY-MM-DD');
    const customFromDateField = form.$('customFromDate');
    const customToDateField = form.$('customToDate');
    const { customFromDate, customToDate } = form.values();
    const customFromDateInnerValue = formatDateValue(
      customFromDate,
      defaultFromDate
    );
    const customToDateInnerValue = formatDateValue(customToDate, defaultToDate);

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
              onReset={() =>
                form.select('customFromDate').set(this.props.fromDate)
              }
            />
          </div>
          <div className={styles.dateRangeInput}>
            <TinyDatePicker
              {...customToDateField.bind()}
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeTo)}
              innerValue={customToDateInnerValue}
              pickerPanelPosition="right"
              onReset={() => form.select('customToDate').set(this.props.toDate)}
            />
          </div>
        </div>
      </div>
    );
  };

  renderAmountRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const { fromAmount, toAmount } = this.props;
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

    return (
      <div className={styles.amountRange}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.amountRange)}</label>
        </div>
        <div className={styles.body}>
          <div className={styles.amountRangeInput}>
            <TinyInput
              type="number"
              {...fromAmountField.bind()}
              useReadMode
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeFrom)}
              innerLabelSuffix={intl.formatMessage(messages.amountUnit)}
              innerValue={fromAmountInnerValue}
            />
          </div>
          <div className={styles.amountRangeInput}>
            <TinyInput
              type="number"
              {...toAmountField.bind()}
              useReadMode
              innerLabelPrefix={intl.formatMessage(globalMessages.rangeTo)}
              innerLabelSuffix={intl.formatMessage(messages.amountUnit)}
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
