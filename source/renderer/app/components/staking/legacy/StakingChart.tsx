import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { BarChart, Bar, YAxis, XAxis, Cell, ReferenceLine } from 'recharts';
import StakingChartTooltip from './StakingChartTooltip';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingChart.scss' or its co... Remove this comment to see the full error message
import styles from './StakingChart.scss';

class CustomReferenceLine extends ReferenceLine {
  getEndPoints(isX, isY) {
    const endPoints = super.getEndPoints(isX, isY);
    const end = endPoints[0];
    const start = endPoints[1];
    end.y += 10;
    end.x -= 7;
    start.x = end.x;
    return endPoints;
  }
}

type Props = {
  width: number;
  height: number;
  options: Record<string, any>;
};
type State = {
  isHovered: boolean;
  hoveredBarData: Record<string, any> | null | undefined;
  tooltipPos?:
    | {
        left: number;
        top: number;
      }
    | null
    | undefined;
};

@observer
class StakingChart extends Component<Props, State> {
  state = {
    isHovered: false,
    hoveredBarData: null,
    tooltipPos: null,
  };

  onMouseMove(event: MouseEvent) {
    if (this.state.isHovered) {
      this.setState({
        tooltipPos: {
          left: event.pageX + 10,
          top: event.pageY,
        },
      });
    }
  }

  render() {
    const { width, height } = this.props;
    const { activeIndex, data, ticks } = this.props.options;
    const { isHovered, hoveredBarData, tooltipPos } = this.state;
    const refLineSlot = Math.floor(data.length / 2) + 1;
    let tooltip = null;

    // TODO: find better way to represent the data records that are behind the reference line
    // for now this is the easiest way to ignore zero-bars in the chart
    if (
      isHovered &&
      hoveredBarData &&
      hoveredBarData.numberOfTransactions > 0
    ) {
      tooltip = (
        <div className={styles.toolTip} style={tooltipPos}>
          <StakingChartTooltip {...hoveredBarData} />
        </div>
      );
    }

    return (
      <div
        className={styles.component}
        onMouseMove={this.onMouseMove.bind(this)}
      >
        <BarChart
          width={width}
          height={height}
          data={data.slice()}
          barSize={10}
          barCategoryGap={2}
          barGap={2}
        >
          <XAxis hide dataKey="slot" />
          <YAxis
            axisLine={false}
            tickLine={false}
            allowDecimals={false}
            ticks={ticks.slice()}
            domain={['dataMin', 'dataMax + 10']}
          />
          {/* @ts-ignore ts-migrate(2607) FIXME: JSX element class does not support attributes beca... Remove this comment to see the full error message */}
          <CustomReferenceLine x={refLineSlot} stroke="#5e6066" />
          <Bar
            dataKey="numberOfTransactions"
            onMouseEnter={(barData) =>
              this.setState({
                isHovered: true,
                hoveredBarData: barData,
              })
            }
            onMouseLeave={() =>
              this.setState({
                isHovered: false,
                hoveredBarData: null,
              })
            }
            minPointSize={2}
            isAnimationActive={false}
          >
            {data.slice().map((entry, index) => {
              let fillColor = '#c2cad4';
              let cursor = 'pointer';
              // eslint-disable-next-line react/no-array-index-key
              if (index === activeIndex) fillColor = '#445b7c';

              if (entry.numberOfTransactions === 0) {
                fillColor = '#e7eaee';
                cursor = 'default';
              }

              return (
                <Cell
                  cursor={cursor}
                  fill={fillColor} // eslint-disable-next-line react/no-array-index-key
                  key={`cell-${index}`}
                />
              );
            })}
          </Bar>
        </BarChart>
        {tooltip}
      </div>
    );
  }
}

export default StakingChart;
