// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import Datetime from 'react-datetime';
import { intlShape } from 'react-intl';
import moment from 'moment';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import globalMessages from '../../../i18n/global-messages';
import TinyButton from './TinyButton';
import TinyInput from './TinyInput';
import styles from './TinyDatePicker.scss';

type PickerPanelPosition = 'left' | 'right';

type Props = $Exact<{
  onBlur?: Function,
  onChange?: Function,
  onReset?: Function,
  onClick?: Function,
  onFocus?: Function,
  onKeyDown?: Function,
  isValidDate?: Function,
  locale?: string,
  dateFormat: string,
  disablePaste?: boolean,
  value: string,
  label?: string,
  placeholder?: string,
  innerLabelPrefix: string,
  innerValue: string | Element<any>,
  pickerPanelPosition: PickerPanelPosition,
  useReadMode?: boolean,
  error?: string | Element<any>,
}>;

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
      onReset, // eslint-disable-line
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
          label={label}
          placeholder={placeholder}
          onChange={(value, evt) => console.log(evt)}
          onInput={(evt) => {
            const inputDate = moment(evt.target.value, dateFormat);
            if (
              !inputDate.isValid() ||
              (isValidDate && !isValidDate(inputDate))
            ) {
              evt.target.value = '';
            }
            // if (props.onInput) {
            //   props.onInput(evt);
            // }
          }}
          value={value ? moment(value).format(dateFormat) : ''}
          disablePaste={disablePaste}
          innerLabelPrefix={innerLabelPrefix}
          innerValue={innerValue}
          useReadMode={useReadMode}
          error={error}
        />
      </PopOver>
    );
    /* eslint-enable */
  }
}
