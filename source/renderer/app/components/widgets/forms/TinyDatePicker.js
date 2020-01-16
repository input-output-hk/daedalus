// @flow
import React, { Component, createRef } from 'react';
import Datetime from 'react-datetime';
import { intlShape } from 'react-intl';
import classNames from 'classnames';
import moment from 'moment';
import globalMessages from '../../../i18n/global-messages';
import TinyInput from './TinyInput';
import styles from './TinyDatePicker.scss';

type PickerPanelPosition = 'left' | 'right';

type Props = {
  onBlur?: Function,
  onChange?: Function,
  onReset?: Function,
  onClick?: Function,
  onFocus?: Function,
  onKeyDown?: Function,
  value: string,
  innerLabelPrefix: string,
  innerValue: any,
  pickerPanelPosition: PickerPanelPosition,
};

export default class TinyDatePicker extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    onReset: () => null,
  };

  selfRef: any;
  resetButtonContainer: any;
  resetButton: any;

  constructor(props: Props) {
    super(props);

    this.selfRef = createRef();
    this.resetButtonContainer = document.createElement('div');
    this.resetButton = document.createElement('button');

    this.resetButton.className = 'SimpleButton_root ButtonOverrides_root';
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
      innerLabelPrefix,
      innerValue,
      pickerPanelPosition,
      ...rest
    } = this.props;
    const componentClassNames = classNames([
      styles.component,
      pickerPanelPosition === 'left' ? styles.pickerPanelOnLeft : null,
      pickerPanelPosition === 'right' ? styles.pickerPanelOnRight : null,
    ]);

    return (
      <div className={componentClassNames} ref={this.selfRef}>
        <Datetime
          timeFormat={false}
          onViewModeChange={this.ensureResetButtonExistence}
          renderInput={props => (
            <TinyInput
              {...props}
              onFocus={(...args) => {
                props.onFocus(...args);
                this.ensureResetButtonExistence();
              }}
              value={moment(props.value).format('DD.MM.YYYY')}
              useReadMode
              innerLabelPrefix={innerLabelPrefix}
              innerValue={innerValue}
            />
          )}
          {...rest}
        />
      </div>
    );
  }
}
