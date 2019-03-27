// const fontLight= 'NotoSans-Light, NotoSansCJKjp-Light';
// const fontMedium= 'NotoSans-Medium, NotoSansCJKjp-Medium';
const fontRegular = 'NotoSans-Regular, NotoSansCJKjp-Regular';
// const fontBold= 'NotoSans-Bold, NotoSansCJKjp-Bold';

const labelStyle = {
  fill: 'rgba(194, 202, 212, 0.6)',
  fontFamily: fontRegular,
  fontSize: 10,
  lineHeight: 1.3,
};

export default {
  bar: {
    fill: 'rgba(68, 91, 124, 0.5)',
  },
  xAxis: {
    tick: {
      angle: 90,
      fontSize: 10,
      fontWeight: 100,
      stroke: '#c2cad4',
      textAnchor: 'start',
      fontFamily: fontRegular,
    },
    label: {
      ...labelStyle,
      position: 'insideBottomRight',
      angle: 90,
    },
  },
  yAxis: {
    tick: {
      fontSize: 10,
      fontWeight: 100,
      stroke: '#c2cad4',
      textAnchor: 'end',
      fontFamily: fontRegular,
    },
    label: {
      ...labelStyle,
      position: 'insideTopLeft',
    },
  },
  cartesianGridBackground: {
    fill: 'rgba(68, 91, 124, 0.06)',
  },
  tooltip: {
    wrapperStyle: {
      // background: 'rgba(94, 96, 102, 0.9)',
    },
    contentStyle: {
      background: 'orange',
      color: '#fafbfc',
      fill: '#fafbfc',
    },
    labelStyle: {
      background: 'pink',
      color: '#fafbfc',
      fill: '#fafbfc',
    },
    cursor: {
      fill: '#445b7c',
    },
  },
};

// #445b7c
