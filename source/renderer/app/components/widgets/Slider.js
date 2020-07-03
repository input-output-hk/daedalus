// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import RcSlider from 'rc-slider';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { shortNumber } from '../../utils/formatters';
import styles from './Slider.scss';

type Props = {
  className?: string,
  min: number,
  max: number,
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
  showTooltip?: boolean,
  minTooltip?: string,
  maxTooltip?: string,
  startPoint?: number,
  tabIndex?: number,
  ariaLabelForHandle?: string,
  ariaLabelledByForHandle?: string,
  ariaValueTextFormatterForHandle?: Function,
};

@observer
export default class Slider extends Component<Props> {
  static defaultProps = {
    min: 0,
    max: 100,
    value: 0,
  };

  render() {
    const { showTooltip, minTooltip, maxTooltip, ...rest } = this.props;
    const { min, max, value } = rest;

    const valueMarkLeftPosition =
      max === min ? `0` : `${((value - min) / (max - min)) * 100}%`;
    const valueMarkStyle = { left: valueMarkLeftPosition };
    const formattedValue = new BigNumber(value).toFormat(0);

    return (
      <div className={styles.component}>
        <div className={styles.upperMarks}>
          <div className={styles.minMark}>
            {showTooltip ? (
              <Tooltip skin={TooltipSkin} tip={minTooltip}>
                {shortNumber(min)}
              </Tooltip>
            ) : (
              shortNumber(min)
            )}
          </div>
          <div className={styles.maxMark}>
            {showTooltip ? (
              <Tooltip skin={TooltipSkin} tip={maxTooltip}>
                {shortNumber(max)}
              </Tooltip>
            ) : (
              shortNumber(max)
            )}
          </div>
        </div>
        <RcSlider {...rest} />
        <div className={styles.lowerMarks}>
          <div className={styles.valueMark} style={valueMarkStyle}>
            {formattedValue}
          </div>
        </div>
      </div>
    );
  }
}
