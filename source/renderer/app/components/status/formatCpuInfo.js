// @flow
import type { Cpu } from '../../types/systemInfoTypes';
import { formattedNumber } from '../../utils/formatters';

const formatCpuInfo = ({ model, speed }: Cpu): string => {
  const modelName = model.split('@')[0].trim().replace(/\s+/g, ' ');
  const speedValue = formattedNumber(speed / 1000, 2);

  return `${modelName} @ ${speedValue}GHz`;
};

export default formatCpuInfo;
