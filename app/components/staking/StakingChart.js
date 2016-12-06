// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxTypes } from 'mobx-react';
import { BarChart, Bar, YAxis, XAxis, Cell, ReferenceLine } from 'recharts';
import styles from './StakingChart.scss';

class CustomReferenceLine extends ReferenceLine {
  getEndPoints(isX, isY) {
    const endPoints = super.getEndPoints(isX, isY);
    const end = endPoints[0];
    const start = endPoints[1];
    end.y += 10;
    end.x = start.x = end.x - 7;
    return endPoints;
  }
}

@observer
export default class StakingChart extends Component {

  static propTypes = {
    options: MobxTypes.observableObject.isRequired,
    onBarClick: PropTypes.func
  };

  render() {
    const { onBarClick } = this.props;
    const { activeIndex, data, ticks } = this.props.options;
    const refLineSlot = Math.floor(data.length / 2) + 1;
    return (
      <div className={styles.component}>
        <BarChart
          width={600}
          height={300}
          data={data.slice()}
          barSize={10}
          barCategoryGap={2}
          barGap={2}
        >
          <XAxis
            hide
            dataKey="slot"
          />
          <YAxis
            axisLine={false}
            tickLine={false}
            allowDecimals={false}
            ticks={ticks.slice()}
            domain={['dataMin', 'dataMax + 10']}
          />
          <CustomReferenceLine
            x={refLineSlot}
            stroke="#5e6066"
          />
          <Bar
            dataKey="transactions"
            onClick={onBarClick}
            minPointSize={2}
            isAnimationActive={false}
          >
            {
              data.slice().map((entry, index) => {
                let fillColor = '#c2cad4';
                if (index === activeIndex) fillColor = '#445b7c';
                if (entry.transactions === 0) fillColor = '#e7eaee';
                return (
                  <Cell
                    cursor="pointer"
                    fill={fillColor}
                    key={`cell-${index}`}
                  />
                );
              })
            }
          </Bar>
        </BarChart>
      </div>
    );
  }

}
