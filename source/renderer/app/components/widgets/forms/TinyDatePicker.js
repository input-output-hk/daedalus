// @flow
import React, { Component, createRef } from 'react';
import type { Element } from 'react';
import Datetime from 'react-datetime';
import { intlShape } from 'react-intl';
import classNames from 'classnames';
import moment from 'moment';
import globalMessages from '../../../i18n/global-messages';
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

  selfRef: any;
  resetButtonContainer: any;
  resetButton: any;

  constructor(props: Props) {
    super(props);

    this.selfRef = createRef();
    this.resetButtonContainer = document.createElement('div');
    this.resetButton = document.createElement('button');

    this.resetButton.className = 'flat SimpleButton_root ButtonOverrides_root';
    this.resetButton.onclick = props.onReset;
    this.resetButtonContainer.className = 'reset TinyButton_component';
    this.resetButtonContainer.appendChild(this.resetButton);
  }

  ensureResetButtonExistence = () => {
    const containerDOMElement = this.selfRef ? this.selfRef.current : null;

    this.resetButton.innerText = this.context.intl.formatMessage(
      globalMessages.reset
    );

    if (containerDOMElement) {
      setTimeout(() => {
        const monthsPanel = containerDOMElement.querySelector('.rdtMonths');
        const daysPanel = containerDOMElement.querySelector('.rdtDays');
        if (monthsPanel && !monthsPanel.lastChild.classList.contains('reset')) {
          monthsPanel.appendChild(this.resetButtonContainer);
        }
        if (daysPanel && !daysPanel.lastChild.classList.contains('reset')) {
          daysPanel.appendChild(this.resetButtonContainer);
        }
      }, 0);
    }
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
    const componentClassNames = classNames([
      styles.component,
      pickerPanelPosition === 'left' ? styles.pickerPanelOnLeft : null,
      pickerPanelPosition === 'right' ? styles.pickerPanelOnRight : null,
    ]);

    /* eslint-disable */
    return (
      <div className={componentClassNames} ref={this.selfRef}>
        <Datetime
          dateFormat={dateFormat}
          timeFormat={false}
          value={value ? moment(value).toDate() : null}
          onViewModeChange={this.ensureResetButtonExistence}
          isValidDate={isValidDate}
          onChange={selectedDate => {
            if (typeof selectedDate === 'string') {
              if (!selectedDate) {
                onChange && onChange(selectedDate);
              }
            } else {
              onChange && onChange(selectedDate.format('YYYY-MM-DD'));
            }
          }}
          renderInput={props => (
            <>
              <TinyInput
                {...props}
                label={label}
                placeholder={placeholder}
                onChange={(value, evt) => props.onChange(evt)}
                onFocus={(...args) => {
                  props.onFocus(...args);
                  this.ensureResetButtonExistence();
                }}
                onInput={evt => {
                  const inputDate = moment(evt.target.value, dateFormat);
                  if (
                    !inputDate.isValid() ||
                    (isValidDate && !isValidDate(inputDate))
                  ) {
                    evt.target.value = '';
                  }
                  if (props.onInput) {
                    props.onInput(evt);
                  }
                }}
                value={value ? moment(value).format(dateFormat) : ''}
                disablePaste={disablePaste}
                innerLabelPrefix={innerLabelPrefix}
                innerValue={innerValue}
                useReadMode={useReadMode}
                error={error}
              />
            </>
          )}
          {...rest}
        />
      </div>
    );
    /* eslint-enable */
  }
}
