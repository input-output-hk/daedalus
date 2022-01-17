import formatCpuInfo from './formatCpuInfo';

describe('Formatters/formatCpuInfo', () => {
  it('gives correct model name and clock speed for Intel format', () => {
    expect(
      formatCpuInfo({
        model: 'Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz',
        speed: 2600,
      })
    ).toEqual('Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz');
  });
  it('gives correct model name and clock speed for AMD format', () => {
    expect(
      formatCpuInfo({
        model: 'AMD Ryzen 7 3700U with Radeon Vega Mobile Gfx',
        speed: 2400,
      })
    ).toEqual('AMD Ryzen 7 3700U with Radeon Vega Mobile Gfx @ 2.40GHz');
  });
  it('removes all unnecessary whitespace characters', () => {
    expect(
      formatCpuInfo({
        // eslint-disable-next-line no-useless-escape
        model: ' Super Fast    NextGen\tProcessor ',
        speed: 1000,
      })
    ).toEqual('Super Fast NextGen Processor @ 1.00GHz');
  });
});
