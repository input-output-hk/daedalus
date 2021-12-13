import React, { Component } from 'react';
// @ts-ignore ts-migrate(2724) FIXME: '"react"' has no exported member named 'Element'. ... Remove this comment to see the full error message
import type { Element } from 'react';
import Datetime from 'react-datetime';
import { intlShape } from 'react-intl';
import moment from 'moment';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import globalMessages from '../../../i18n/global-messages';
import TinyButton from './TinyButton';
import TinyInput from './TinyInput';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './TinyDatePicker.scss' or its ... Remove this comment to see the full error message
import styles from './TinyDatePicker.scss';

type PickerPanelPosition = 'left' | 'right';
type Props = {
  onBlur?: (...args: Array<any>) => any;
  onChange?: (...args: Array<any>) => any;
  onReset?: (...args: Array<any>) => any;
  onClick?: (...args: Array<any>) => any;
  onFocus?: (...args: Array<any>) => any;
  onKeyDown?: (...args: Array<any>) => any;
  isValidDate?: (...args: Array<any>) => any;
  locale?: string;
  dateFormat: string;
  disablePaste?: boolean;
  value: string;
  label?: string | Element<any>;
  placeholder?: string;
  innerLabelPrefix: string;
  innerValue: string | Element<any>;
  pickerPanelPosition: PickerPanelPosition;
  useReadMode?: boolean;
  error?: string | Element<any>;
};
export default class TinyDatePicker extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    onReset: () => null,
    onChange: () => null,
    locale: 'en-us',
  };

  render() {
    const {
      onReset,
      onChange,
      isValidDate,
      dateFormat,
      disablePaste,
      value,
      label,
      placeholder,
      innerLabelPrefix,
      innerValue,
      pickerPanelPosition,
      useReadMode,
      error,
      ...rest
    } = this.props;

    /* eslint-disable */
    return (
      <PopOver
        interactive
        arrow={false}
        placement="auto"
        duration={0}
        trigger="click"
        content={
          <div className={styles.datePicker}>
            <Datetime
              open
              input={false}
              dateFormat={dateFormat}
              timeFormat={false}
              value={value ? moment(value).toDate() : null}
              isValidDate={isValidDate}
              onChange={(selectedDate) => {
                if (typeof selectedDate === 'string') {
                  if (!selectedDate) {
                    onChange && onChange(selectedDate);
                  }
                } else {
                  onChange && onChange(selectedDate.format('YYYY-MM-DD'));
                }
              }}
              {...rest}
            />
            <TinyButton
              containerClassName={styles.resetButton}
              onClick={onReset}
              label={this.context.intl.formatMessage(globalMessages.reset)}
              disabled={value == null || value === ''}
            />
          </div>
        }
      >
        <TinyInput
          autoFocus={false}
          value={value ? moment(value).format(dateFormat) : ''}
          label={label}
          placeholder={placeholder}
          innerLabelPrefix={innerLabelPrefix}
          onChange={(value, evt) => console.log(evt)}
          useReadMode={useReadMode}
          innerValue={innerValue}
          error={error}
        />
      </PopOver>
    );
  }
}
