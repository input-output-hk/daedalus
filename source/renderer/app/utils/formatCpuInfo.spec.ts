import formatCpuInfo from './formatCpuInfo';

describe('Formatters/formatCpuInfo', () => {
  it('gives correct model name and clock speed for Intel format', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2741) FIXME: Property 'times' is missing in type '{ model: stri... Remove this comment to see the full error message
        {
          model: 'Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz',
          speed: 2600,
        },
      ])
    ).toEqual('Intel(R) Core(TM) i7-9750H CPU @ 2.60GHz');
  });
  it('gives correct model name and clock speed for AMD format', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2741) FIXME: Property 'times' is missing in type '{ model: stri... Remove this comment to see the full error message
        {
          model: 'AMD Ryzen 7 3700U with Radeon Vega Mobile Gfx',
          speed: 2400,
        },
      ])
    ).toEqual('AMD Ryzen 7 3700U with Radeon Vega Mobile Gfx @ 2.40GHz');
  });
  it('removes all unnecessary whitespace characters', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2741) FIXME: Property 'times' is missing in type '{ model: stri... Remove this comment to see the full error message
        {
          model: ' Super Fast    NextGen\tProcessor ',
          speed: 1000,
        },
      ])
    ).toEqual('Super Fast NextGen Processor @ 1.00GHz');
  });
  it('returns empty string if there is no any thread data present', () => {
    expect(formatCpuInfo([])).toEqual('');
  });
  it('returns empty string if there is no model property present', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2739) FIXME: Type '{ speed: number; }' is missing the following... Remove this comment to see the full error message
        {
          speed: 1000,
        },
      ])
    ).toEqual('');
  });
  it('returns model name if there is no speed property present', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2739) FIXME: Type '{ model: string; }' is missing the following... Remove this comment to see the full error message
        {
          model: 'Model name',
        },
      ])
    ).toEqual('Model name');
  });
  it('returns parsed model if there is no speed property present but the model property contains speed expressed in GHz', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2739) FIXME: Type '{ model: string; }' is missing the following... Remove this comment to see the full error message
        {
          model: 'Model name @ 0.5 GHz',
        },
      ])
    ).toEqual('Model name @ 0.50GHz');
  });
  it('returns model name if there is no speed property present and neither speed correctly expressed in the model', () => {
    expect(
      formatCpuInfo([
        // @ts-ignore ts-migrate(2739) FIXME: Type '{ model: string; }' is missing the following... Remove this comment to see the full error message
        {
          model: 'Model name @ 1 MHz',
        },
      ])
    ).toEqual('Model name');
  });
});
