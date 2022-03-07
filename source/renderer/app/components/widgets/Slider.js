// @flow
import React, { useState } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import RcSlider from 'rc-slider';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { shortNumber } from '../../utils/formatters';
import styles from './Slider.scss';

type Props = {
  className?: string,
  min: number,
  minDisplayValue?: number,
  max: number,
  maxDisplayValue?: number,
  marks?: any,
  step?: number,
  vertical?: boolean,
  handle?: Function,
  included?: boolean,
  reverse?: boolean,
  disabled?: boolean,
  dots?: boolean,
  onBeforeChange?: Function,
  onChange: Function,
  onAfterChange?: Function,
  minimumTrackStyle?: any,
  maximumTrackStyle?: any,
  handleStyle?: any,
  trackStyle?: any,
  railStyle?: any,
  dotStyle?: any,
  activeDotStyle?: any,
  defaultValue?: number,
  value: number,
  displayValue?: any,
  showRawValue?: boolean,
  showTooltip?: boolean,
  minTooltip?: string,
  maxTooltip?: string,
  startPoint?: number,
  tabIndex?: number,
  ariaLabelForHandle?: string,
  ariaLabelledByForHandle?: string,
  ariaValueTextFormatterForHandle?: Function,
};

type State = {
  initialValue: number,
};

@observer
export default class Slider extends Component<Props, State> {
  static defaultProps = {
    min: 0,
    max: 100,
    value: 0,
  };
  state: State = {
    initialValue: 0,
  };

  const valueMarkLeftPosition =
    max === min ? `0` : `${((value - min) / (max - min)) * 100}%`;
  const valueMarkStyle = { left: valueMarkLeftPosition };
  const formattedValue = showRawValue
    ? displayValue || value
    : new BigNumber(value).toFormat(0);

  return (
    <div className={styles.component}>
      <div className={styles.upperMarks}>
        <div className={styles.minMark}>
          {showTooltip ? (
            <PopOver content={minTooltip}>
              {shortNumber(minDisplayValue || min)}
            </PopOver>
          ) : (
            shortNumber(minDisplayValue || min)
          )}
        </div>
        <div className={styles.maxMark}>
          {showTooltip ? (
            <PopOver content={maxTooltip}>
              {shortNumber(maxDisplayValue || max)}
            </PopOver>
          ) : (
            shortNumber(maxDisplayValue || max)
          )}
        </div>
        <RcSlider
          {...rest}
          onBeforeChange={(e) => this.setState({ initialValue: e })}
          onAfterChange={(e) => {
            if (e !== this.state.initialValue && rest.onAfterChange) {
              rest.onAfterChange(e);
            }
          }}
        />
        <div className={styles.lowerMarks}>
          <div className={styles.valueMark} style={valueMarkStyle}>
            {formattedValue}
          </div>
        </div>
      </div>
    </div>
  );
});
