import { getShortCpuDescription } from './getShortCpuDescription';

const appleCpus = [
  'Apple M1',
  'Apple M1 Max',
  'Apple M1 Pro',
  'Apple M1 Ultra',
  'Apple M2',
  'VirtualApple',
];

const intelI3Cpus = [
  '11th Gen Intel(R) Core(TM) i3-1115G4',
  'Intel(R) Core(TM) i3 CPU 550',
  'Intel(R) Core(TM) i3-7300 CPU',
  'Intel(R) Core(TM) i3-9100T CPU',
];

const intelI5Cpus = [
  '11th Gen Intel(R) Core(TM) i5-11400',
  '11th Gen Intel(R) Core(TM) i5-1145G7',
  '12th Gen Intel(R) Core(TM) i5-12600K',
  'Intel(R) Core(TM) i5 CPU 650',
  'Intel(R) Core(TM) i5-10210U CPU',
  'Intel(R) Core(TM) i5-1030NG7 CPU',
];

const intelI7Cpus = [
  '11th Gen Intel(R) Core(TM) i7-11370H',
  'Intel(R) Core(TM) i7 CPU 870',
  'Intel(R) Core(TM) i7-10510U CPU',
];

const intelI9Cpus = [
  '11th Gen Intel(R) Core(TM) i9-11900KF',
  '12th Gen Intel(R) Core(TM) i9-12900KF',
  '13th Gen Intel(R) Core(TM) i9-13900K',
  'Intel(R) Core(TM) i9-9880H CPU',
];

const intelCeleronCpus = [
  'Intel(R) Celeron(R) 6305',
  'Intel(R) Celeron(R) CPU N3450',
  'Intel(R) Celeron(R) J4115 CPU',
  'Intel(R) Celeron(R) N5105',
];

const intelXeonCpus = [
  'Intel(R) Xeon(R) CPU E3-1220 v3',
  'Intel(R) Xeon(R) CPU E3-1225 V2',
  'Intel(R) Xeon(R) CPU E31230',
  'Intel(R) Xeon(R) W-3265M CPU',
];

const intelPentiumCpus = [
  'Intel(R) Pentium(R) 3556U',
  'Intel(R) Pentium(R) CPU 4417U',
  'Intel(R) Pentium(R) Gold 7505',
  'Intel(R) Pentium(R) Gold G5420 CPU',
  'Intel(R) Pentium(R) Silver N5030 CPU',
];

const otherIntelCpus = [
  'Genuine Intel(R) CPU 0000',
  'Intel(R) Atom(TM) CPU C3538',
  'Intel(R) Core(TM) M-5Y31 CPU',
  'Intel(R) Core(TM) m3-6Y30 CPU',
  'Intel(R) Core(TM)2 Duo CPU E7500',
  'Intel(R) Core(TM)2 Quad CPU Q6700',
];

const amdRyzen3Cpus = [
  'AMD Ryzen 3 1200 Quad-Core Processor',
  'AMD Ryzen 3 2200G with Radeon Vega Graphics',
  'AMD Ryzen 3 5300U with Radeon Graphics',
];

const amdRyzen5Cpus = [
  'AMD Ryzen 5 1500X Quad-Core Processor',
  'AMD Ryzen 5 2500U with Radeon Vega Mobile Gfx',
  'AMD Ryzen 5 5500',
  'AMD Ryzen 5 PRO 4650G with Radeon Graphics',
];

const amdRyzen7Cpus = [
  'AMD Ryzen 7 1700 Eight-Core Processor',
  'AMD Ryzen 7 2700U with Radeon Vega Mobile Gfx',
  'AMD Ryzen 7 5800 8-Core Processor',
  'AMD Ryzen 7 Extreme Edition',
  'AMD Ryzen 7 Microsoft Surface (R) Edition',
];

const amdRyzen9Cpus = [
  'AMD Ryzen 9 3900 12-Core Processor',
  'AMD Ryzen 9 5950X 16-Core Processor',
  'AMD Ryzen 9 5980HS with Radeon Graphics',
];

const amdRyzenThreadripperCpus = [
  'AMD Ryzen Threadripper 1920X 12-Core Processor',
  'AMD Ryzen Threadripper 2990WX 32-Core Processor',
];

const amdA6Cpus = [
  'AMD A6-7310 APU with AMD Radeon R4 Graphics',
  'AMD A6-5200 APU with Radeon(TM) HD Graphics',
];

const amdA8Cpus = [
  'AMD A8-3850 APU with Radeon(tm) HD Graphics',
  'AMD A8-7410 APU with AMD Radeon R5 Graphics',
  'AMD A8-7650K Radeon R7',
];

const amdA10Cpus = [
  'AMD A10-6800K APU with Radeon(tm) HD Graphics',
  'AMD A10-7850K Radeon R7',
  'AMD A10-8700P Radeon R6',
  'AMD A10-9600P RADEON R5',
];

const amdA12Cpus = ['AMD A12-9720P RADEON R7'];

const amdFXCpus = [
  'AMD FX(tm)-6300 Six-Core Processor',
  'AMD FX(tm)-8350 Eight-Core Processor',
  'AMD FX-8320E Eight-Core Processor',
];

const otherAmdCpus = [
  'AMD 3020e with Radeon Graphics',
  'AMD E2-9000 RADEON R2',
];

const otherCpus = ['Common KVM processor', 'Unknown', ''];

describe('Formatters/formatCpuInfo', () => {
  it('leaves apple processors unchanged', () => {
    appleCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual(cpu)
    );
  });

  it('labels Intel Core i3-i9 processors"', () => {
    intelI3Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Core i3')
    );
    intelI5Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Core i5')
    );
    intelI7Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Core i7')
    );
    intelI9Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Core i9')
    );

    // future-proofing
    expect(getShortCpuDescription('Intel(R) Core(TM) i13')).toEqual(
      'Intel Core i13'
    );
  });

  it('labels Intel Pentium/Celeron/Xeon processors"', () => {
    intelPentiumCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Pentium')
    );
    intelCeleronCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Celeron')
    );
    intelXeonCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Intel Xeon')
    );
  });

  it('labels other Intel processors as "Other Intel"', () => {
    otherIntelCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Other Intel')
    );
  });

  it('labels AMD Ryzen processors"', () => {
    amdRyzen3Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD Ryzen 3')
    );
    amdRyzen5Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD Ryzen 5')
    );
    amdRyzen7Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD Ryzen 7')
    );
    amdRyzen9Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD Ryzen 9')
    );

    // future-proofing
    expect(getShortCpuDescription('AMD Ryzen 13 2000')).toEqual('AMD Ryzen 13');
  });

  it('labels AMD Athlon/Ryzen Threadripper/EPYC/etc. processors', () => {
    amdRyzenThreadripperCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD Ryzen Threadripper')
    );
    amdFXCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD FX')
    );
    expect(
      getShortCpuDescription('AMD Ryzen Threadripper PRO 3955WX 16-Cores')
    ).toEqual('AMD Ryzen Threadripper PRO');
    expect(getShortCpuDescription('AMD EPYC 7402P 24-Core Processor')).toEqual(
      'AMD EPYC'
    );
    expect(getShortCpuDescription('AMD PRO A8-8600B R6')).toEqual('AMD PRO');
    expect(
      getShortCpuDescription('AMD Ryzen Embedded V1605B with Radeon Vega Gfx')
    ).toEqual('AMD Ryzen Embedded');
    expect(
      getShortCpuDescription('AMD Phenom(tm) 9550 Quad-Core Processor')
    ).toEqual('AMD Phenom');
    expect(
      getShortCpuDescription('AMD Sempron(tm) 3850 APU with Radeon(tm) R3')
    ).toEqual('AMD Sempron');
    expect(
      getShortCpuDescription('AMD Athlon 200GE with Radeon Vega Graphics')
    ).toEqual('AMD Athlon');
  });

  it('labels AMD A* processors', () => {
    amdA6Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD A6')
    );
    amdA8Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD A8')
    );
    amdA10Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD A10')
    );
    amdA12Cpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('AMD A12')
    );
    expect(getShortCpuDescription('AMD A50-1234')).toEqual('AMD A50');
  });

  it('labels other AMD processors as "Other AMD"', () => {
    otherAmdCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Other AMD')
    );
  });

  it('labels other processors as "Other"', () => {
    otherCpus.forEach((cpu) =>
      expect(getShortCpuDescription(cpu)).toEqual('Other')
    );
  });
});
