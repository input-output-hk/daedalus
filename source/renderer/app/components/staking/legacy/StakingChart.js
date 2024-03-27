'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const recharts_1 = require('recharts');
const StakingChartTooltip_1 = __importDefault(require('./StakingChartTooltip'));
const StakingChart_scss_1 = __importDefault(require('./StakingChart.scss'));
class CustomReferenceLine extends recharts_1.ReferenceLine {
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
let StakingChart = class StakingChart extends react_1.Component {
  state = {
    isHovered: false,
    hoveredBarData: null,
    tooltipPos: null,
  };
  onMouseMove(event) {
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
      tooltip = react_1.default.createElement(
        'div',
        { className: StakingChart_scss_1.default.toolTip, style: tooltipPos },
        react_1.default.createElement(StakingChartTooltip_1.default, {
          ...hoveredBarData,
        })
      );
    }
    return react_1.default.createElement(
      'div',
      {
        className: StakingChart_scss_1.default.component,
        onMouseMove: this.onMouseMove.bind(this),
      },
      react_1.default.createElement(
        recharts_1.BarChart,
        {
          width: width,
          height: height,
          data: data.slice(),
          barSize: 10,
          barCategoryGap: 2,
          barGap: 2,
        },
        react_1.default.createElement(recharts_1.XAxis, {
          hide: true,
          dataKey: 'slot',
        }),
        react_1.default.createElement(recharts_1.YAxis, {
          axisLine: false,
          tickLine: false,
          allowDecimals: false,
          ticks: ticks.slice(),
          domain: ['dataMin', 'dataMax + 10'],
        }),
        react_1.default.createElement(CustomReferenceLine, {
          x: refLineSlot,
          stroke: '#5e6066',
        }),
        react_1.default.createElement(
          recharts_1.Bar,
          {
            dataKey: 'numberOfTransactions',
            onMouseEnter: (barData) =>
              this.setState({
                isHovered: true,
                hoveredBarData: barData,
              }),
            onMouseLeave: () =>
              this.setState({
                isHovered: false,
                hoveredBarData: null,
              }),
            minPointSize: 2,
            isAnimationActive: false,
          },
          data.slice().map((entry, index) => {
            let fillColor = '#c2cad4';
            let cursor = 'pointer';
            // eslint-disable-next-line react/no-array-index-key
            if (index === activeIndex) fillColor = '#445b7c';
            if (entry.numberOfTransactions === 0) {
              fillColor = '#e7eaee';
              cursor = 'default';
            }
            return react_1.default.createElement(recharts_1.Cell, {
              cursor: cursor,
              fill: fillColor,
              key: `cell-${index}`,
            });
          })
        )
      ),
      tooltip
    );
  }
};
StakingChart = __decorate([mobx_react_1.observer], StakingChart);
exports.default = StakingChart;
//# sourceMappingURL=StakingChart.js.map
