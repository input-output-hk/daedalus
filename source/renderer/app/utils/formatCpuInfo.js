// @flow
import type {
  Cpu,
  CpuThreadData,
} from '../../../common/types/environment.types';
import { formattedNumber } from './formatters';

const formatCpuInfo = (cpu: Cpu): string => {
  const { model, speed } = cpu?.[0] || ({}: CpuThreadData);
  if (!model) return '';

  const [modelName, modelSpeedSection] = model
    .split('@')
    .map((stringPart) => stringPart.trim().replace(/\s+/g, ' '));
  if (!speed) {
    const modelSpeedSectionExpressedInGHz = !!modelSpeedSection?.match(
      /GHz/
    )?.[0];
    if (!modelSpeedSection && !modelSpeedSectionExpressedInGHz)
      return modelName;

    const rawSpeedValue = modelSpeedSection.match(/([\d,.]+)\s*GHz/)?.[1];
    const parsedSpeedValue = parseFloat(rawSpeedValue);
    if (!parsedSpeedValue) return modelName;

    const speedValue = formattedNumber(parsedSpeedValue, 2);
    return `${modelName} @ ${speedValue}GHz`;
  }

  const speedValue = formattedNumber(speed / 1000, 2);
  return `${modelName} @ ${speedValue}GHz`;
};

export default formatCpuInfo;
