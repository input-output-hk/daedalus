import { Environment } from '../../../common/types/environment.types';
import {
  CPU_DIMENSION_KEY,
  OS_DIMENSION_KEY,
  RAM_DIMENSION_KEY,
  USES_HARDWARE_WALLET_DIMENSION_KEY,
  USES_LEGACY_WALLET_DIMENSION_KEY,
  VERSION_DIMENSION_KEY,
} from '../config/analyticsConfig';
import { getShortCpuDescription } from '../utils/getShortCpuDescription';
import { formattedBytesToSize } from '../utils/formatters';
import AdaApi from '../api/api';

const booleanToText = (flag: boolean) => (flag ? 'yes' : 'no');

export const getCustomDimensions = async (
  environment: Environment,
  adaApi: AdaApi
) => {
  const userWallets = await adaApi.getWallets();
  const usesByronWallets = userWallets.some((wallet) => wallet.isLegacy);
  const usesHardwareWallets = userWallets.some(
    (wallet) => wallet.isHardwareWallet
  );

  return {
    [CPU_DIMENSION_KEY]: getShortCpuDescription(environment.cpu[0]?.model),
    [RAM_DIMENSION_KEY]: formattedBytesToSize(environment.ram, 0),
    [OS_DIMENSION_KEY]: environment.os,
    [VERSION_DIMENSION_KEY]: environment.version,
    [USES_LEGACY_WALLET_DIMENSION_KEY]: booleanToText(usesByronWallets),
    [USES_HARDWARE_WALLET_DIMENSION_KEY]: booleanToText(usesHardwareWallets),
  };
};
