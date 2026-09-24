const intelCoreRegex = /Intel\(R\) Core\(TM\) i([0-9]+)/;
const olderIntelCpuRegex = /Intel\(R\) (\w+)\(R\)/;
const amdRyzenRegex = /AMD Ryzen ([0-9]+)/;
const amdARegex = /AMD A([0-9]+)/;
const otherAmdCpuRegex = /AMD ([A-Za-z][A-Za-z\s]*[A-Za-z]|[A-Za-z])[\s|(-]/;

export const getShortCpuDescription = (cpu: string | undefined): string => {
  if (!cpu) {
    return 'Other';
  }

  if (cpu.includes('Apple')) {
    return cpu;
  }

  const intelCoreRegexMatch = cpu.match(intelCoreRegex);

  if (intelCoreRegexMatch !== null) {
    const generation = intelCoreRegexMatch[1];
    return `Intel Core i${generation}`;
  }

  const olderIntelCpuRegexMatch = cpu.match(olderIntelCpuRegex);

  if (olderIntelCpuRegexMatch !== null) {
    const name = olderIntelCpuRegexMatch[1];
    return `Intel ${name}`;
  }

  const amdRyzenRegexMatch = cpu.match(amdRyzenRegex);

  if (amdRyzenRegexMatch !== null) {
    const generation = amdRyzenRegexMatch[1];
    return `AMD Ryzen ${generation}`;
  }

  const amdARegexMatch = cpu.match(amdARegex);

  if (amdARegexMatch !== null) {
    const generation = amdARegexMatch[1];
    return `AMD A${generation}`;
  }

  const otherAmdCpuRegexMatch = cpu.match(otherAmdCpuRegex);

  if (otherAmdCpuRegexMatch !== null) {
    const name = otherAmdCpuRegexMatch[1];
    return `AMD ${name}`;
  }

  if (cpu.includes('Intel')) {
    return 'Other Intel';
  }

  if (cpu.includes('AMD')) {
    return 'Other AMD';
  }

  return 'Other';
};
