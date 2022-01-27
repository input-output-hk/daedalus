import type {
  Cpu,
  CpuThreadData,
} from '../../../common/types/environment.types';
import { formattedNumber } from './formatters';
type FormatArgs = {
  modelName: string;
  speedValue: string;
};

const format = ({ modelName, speedValue }: FormatArgs) =>
  `${modelName} @ ${speedValue}GHz`;

const formatCpuInfo = (cpu: Cpu): string => {
  const { model, speed } = cpu?.[0] || ({} as CpuThreadData);
  if (!model) return '';
  const [modelName, modelSpeedSection] = model
    .split('@')
    .map((stringPart) => stringPart.trim().replace(/\s+/g, ' '));

  if (speed) {
    const speedValue = formattedNumber(speed / 1000, 2);
    return format({
      modelName,
      speedValue,
    });
  }

  const modelSpeedSectionExpressedInGHz = !!modelSpeedSection?.match(/GHz/);

  if (!modelSpeedSection && !modelSpeedSectionExpressedInGHz) {
    return modelName;
  }

  const rawSpeedValue = modelSpeedSection.match(/([\d,.]+)\s*GHz/)?.[1];
  const parsedSpeedValue = parseFloat(rawSpeedValue);
  if (!parsedSpeedValue) return modelName;
  const speedValue = formattedNumber(parsedSpeedValue, 2);
  return format({
    modelName,
    speedValue,
  });
};

export default formatCpuInfo;
