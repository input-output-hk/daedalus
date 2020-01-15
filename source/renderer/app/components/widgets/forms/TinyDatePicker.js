// @flow
import React, { Component } from 'react';
import Datetime from 'react-datetime';
import classNames from 'classnames';
import moment from 'moment';
import TinyInput from './TinyInput';
import styles from './TinyDatePicker.scss';

type PickerPanelPosition = 'left' | 'right';

type Props = {
  onBlur?: Function,
  onChange?: Function,
  onClick?: Function,
  onFocus?: Function,
  onKeyDown?: Function,
  value: string,
  innerLabelPrefix: string,
  innerValue: any,
  pickerPanelPosition: PickerPanelPosition,
};

export default class TinyDatePicker extends Component<Props> {
  render() {
    const {
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
      <div className={componentClassNames}>
        <Datetime
          timeFormat={false}
          renderInput={props => (
            <TinyInput
              {...props}
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
