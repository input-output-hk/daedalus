import React, { useState } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import RcSlider from 'rc-slider';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { shortNumber } from '../../utils/formatters';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './Slider.scss' or its correspo... Remove this comment to see the full error message
import styles from './Slider.scss';

type Props = {
  className?: string;
  min: number;
  minDisplayValue?: number;
  max: number;
  maxDisplayValue?: number;
  marks?: any;
  step?: number;
  vertical?: boolean;
  handle?: (...args: Array<any>) => any;
  included?: boolean;
  reverse?: boolean;
  disabled?: boolean;
  dots?: boolean;
  onBeforeChange?: (...args: Array<any>) => any;
  onChange: (...args: Array<any>) => any;
  onAfterChange?: (...args: Array<any>) => any;
  minimumTrackStyle?: any;
  maximumTrackStyle?: any;
  handleStyle?: any;
  trackStyle?: any;
  railStyle?: any;
  dotStyle?: any;
  activeDotStyle?: any;
  defaultValue?: number;
  value: number;
  displayValue?: any;
  showRawValue?: boolean;
  showTooltip?: boolean;
  minTooltip?: string;
  maxTooltip?: string;
  startPoint?: number;
  tabIndex?: number;
  ariaLabelForHandle?: string;
  ariaLabelledByForHandle?: string;
  ariaValueTextFormatterForHandle?: (...args: Array<any>) => any;
};
export const Slider = observer((props: Props) => {
  const [initialValue, setInitialValue] = useState<number | null>(null);
  const {
    showTooltip,
    minTooltip,
    maxTooltip,
    minDisplayValue,
    maxDisplayValue,
    displayValue,
    showRawValue,
    ...rest
  } = props;
  const { min, max, value } = rest;
  const valueMarkLeftPosition =
    max === min ? `0` : `${((value - min) / (max - min)) * 100}%`;
  const valueMarkStyle = {
    left: valueMarkLeftPosition,
  };
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
      </div>
      {/* @ts-ignore ts-migrate(2322) FIXME: Type '{ onBeforeChange: (e: number) => void; onCha... Remove this comment to see the full error message */}
      <RcSlider
        {...rest}
        onBeforeChange={(e: number) => {
          if (!initialValue) setInitialValue(e);
        }}
        onChange={(e: number) => {
          rest.onChange(e);
        }}
        onAfterChange={(e: number) => {
          if (e !== initialValue && !!rest.onAfterChange) {
            rest.onAfterChange(e);
          }

          setInitialValue(null);
        }}
      />
      <div className={styles.lowerMarks}>
        <div className={styles.valueMark} style={valueMarkStyle}>
          {formattedValue}
        </div>
      </div>
    </div>
  );
});
