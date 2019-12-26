// @flow
/* eslint-disable jsx-a11y/label-has-associated-control, jsx-a11y/label-has-for */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import BigNumber from 'bignumber.js';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import TinySelect from '../../widgets/forms/TinySelect';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import globalMessages from '../../../i18n/global-messages';
import filterIcon from '../../../assets/images/filter-dis-ic.inline.svg';
import styles from './Filter.scss';

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

const calculateDateRange = (
  dateRange: string,
  customDateRange: { customFromDate: string, customToDate: string }
) => {
  const { customFromDate, customToDate } = customDateRange;
  let fromDate = null;
  let toDate = null;

  if (dateRange === messages.customDateRange.id) {
    fromDate = customFromDate;
    toDate = customToDate;
  } else {
    if (dateRange === messages.thisWeek.id) {
      fromDate = moment().startOf('week');
    } else if (dateRange === messages.thisMonth.id) {
      fromDate = moment().startOf('month');
    } else if (dateRange === messages.thisYear.id) {
      fromDate = moment().startOf('year');
    } else {
      fromDate = moment();
    }
    fromDate = fromDate.format('YYYY-MM-DD');
    toDate = moment().format('YYYY-MM-DD');
  }

  return { fromDate, toDate };
};

type Props = {
  minDate?: number,
  maxDate?: number,
  minAmount?: BigNumber,
  maxAmount?: BigNumber,
  onFilter: Function,
};

@observer
export default class Filter extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  dateRangeOptions = [
    {
      label: this.context.intl.formatMessage(messages.thisWeek),
      value: messages.thisWeek.id,
    },
    {
      label: this.context.intl.formatMessage(messages.thisMonth),
      value: messages.thisMonth.id,
    },
    {
      label: this.context.intl.formatMessage(messages.thisYear),
      value: messages.thisYear.id,
    },
    {
      label: this.context.intl.formatMessage(messages.customDateRange),
      value: messages.customDateRange.id,
    },
  ];

  form = new ReactToolboxMobxForm({
    fields: {
      incomingChecked: {
        type: 'checkbox',
        label: this.context.intl.formatMessage(messages.incoming),
        value: true,
      },
      outgoingChecked: {
        type: 'checkbox',
        label: this.context.intl.formatMessage(messages.outgoing),
        value: true,
      },
      dateRange: {
        label: this.context.intl.formatMessage(messages.dateRange),
        value: messages.thisWeek.id,
      },
      customFromDate: {
        type: 'date',
        label: '',
        value: this.props.minDate
          ? moment(this.props.minDate).format('YYYY-MM-DD')
          : null,
      },
      customToDate: {
        type: 'date',
        label: '',
        value: this.props.maxDate
          ? moment(this.props.maxDate).format('YYYY-MM-DD')
          : null,
      },
      fromAmount: {
        type: 'number',
        label: '',
        value: this.props.minAmount ? this.props.minAmount.toNumber() : null,
      },
      toAmount: {
        type: 'number',
        label: '',
        value: this.props.maxAmount ? this.props.maxAmount.toNumber() : null,
      },
    },
  });

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
          fromAmount: new BigNumber(rest.fromAmount),
          toAmount: new BigNumber(rest.toAmount),
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
    const customFromDateField = form.$('customFromDate');
    const customToDateField = form.$('customToDate');

    return (
      <div className={styles.customDateRange}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.customDateRange)}</label>
          <DialogCloseButton
            onClick={() => form.get('dateRange').set(this.dateRangeOptions[1])}
          />
        </div>
        <div className={styles.body}>
          <Input type="date" {...customFromDateField.bind()} skin={InputSkin} />
          <Input type="date" {...customToDateField.bind()} skin={InputSkin} />
        </div>
      </div>
    );
  };

  renderAmountRangeField = () => {
    const { form } = this;
    const { intl } = this.context;
    const fromAmountField = form.$('fromAmount');
    const toAmountField = form.$('toAmount');

    return (
      <div className={styles.amountRange}>
        <div className={styles.header}>
          <label>{intl.formatMessage(messages.amountRange)}</label>
        </div>
        <div className={styles.body}>
          <Input type="number" {...fromAmountField.bind()} skin={InputSkin} />
          <Input type="number" {...toAmountField.bind()} skin={InputSkin} />
        </div>
      </div>
    );
  };

  renderActionButton = () => (
    <div className={styles.action}>
      <Button
        label={this.context.intl.formatMessage(messages.filter)}
        onClick={this.handleSubmit}
        skin={ButtonSkin}
      />
    </div>
  );

  render() {
    const { intl } = this.context;
    const { dateRange } = this.form.values();
    const isCustomDateRangeSelected = dateRange === messages.customDateRange.id;

    return (
      <div className={styles.filter}>
        <div className={styles.title}>
          <h4 className={styles.titleText}>
            {intl.formatMessage(messages.filterBy)}
          </h4>
          <button className={styles.titleLink} onClick={() => null}>
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
    );
  }
}
