import { ApiError } from '../../common/cip30/errors';
import { DappCip30Method } from '../../common/cip30/wire';
import {
  HARDWARE_CONNECTOR_MATRIX_REVISION,
  hardwareConnectorRowId,
} from '../../common/types/hardware-wallets.types';
import type { HardwareConnectorActivation } from '../../common/types/hardware-wallets.types';
import { DappLaunchPolicy } from '../dapp/DappLaunchPolicy';
import { ExtensionRegistry, ResolvedMethod } from './ExtensionRegistry';
import { logger } from '../utils/logging';
import { ExtensionDescriptor } from './extensions';

export type DappWalletKind = 'shelley-software' | 'ledger' | 'trezor' | 'byron';

export type CapabilityContext = Readonly<{
  walletKind: DappWalletKind;
  backendApiVersion: number;
  backendExtensions: readonly number[];
  networkSupported: boolean;
  device?: HardwareConnectorActivation;
  policy: DappLaunchPolicy;
}>;

const refused = (): ApiError => ({ code: -3, info: 'Refused' });
const hasText = (value: string): boolean => value.length > 0;

export class CapabilityService {
  constructor(private readonly registry: ExtensionRegistry) {}

  isBaseSupported(context: CapabilityContext): boolean {
    return (
      context.walletKind !== 'byron' &&
      context.backendApiVersion === 1 &&
      context.networkSupported
    );
  }

  isSupported(cip: number, context: CapabilityContext): boolean {
    const descriptor = this.registry.get(cip);
    if (!descriptor || !this.isBaseSupported(context)) return false;
    if (!this.policyAllows(descriptor, context.policy)) return false;
    if (descriptor.availability === 'disabled') return false;
    if (
      descriptor.requiresBackend &&
      !context.backendExtensions.includes(cip)
    ) {
      return false;
    }
    return (
      !descriptor.requiresDevice ||
      this.deviceRefusalReasons(cip, context).length === 0
    );
  }

  isEnabled(cip: number, enabledExtensions: readonly number[]): boolean {
    return this.registry.isKnown(cip) && enabledExtensions.includes(cip);
  }

  requireInvocation(
    method: DappCip30Method,
    enabledExtensions: readonly number[],
    context: CapabilityContext
  ): ResolvedMethod {
    if (!this.isBaseSupported(context)) throw refused();
    const resolved = this.registry.resolve(method, enabledExtensions);
    if (!resolved) throw refused();

    const cip = resolved.extension ?? resolved.override;
    if (resolved.descriptor.requiresDevice) {
      const reasons = this.deviceRefusalReasons(cip, context);
      if (reasons.length > 0) {
        logger.warn('CIP-30 hardware capability refused', {
          method,
          walletKind: context.walletKind,
          requestedCip: cip,
          reasons,
          device: context.device ?? null,
        });
        throw refused();
      }
    }
    if (cip !== undefined && !this.isSupported(cip, context)) throw refused();

    const composition = this.registry.compositionTarget(
      method,
      enabledExtensions
    );
    const compositionCip = composition?.extension ?? composition?.override;
    if (
      compositionCip !== undefined &&
      !this.isSupported(compositionCip, context)
    ) {
      throw refused();
    }
    return resolved;
  }

  private policyAllows(
    descriptor: ExtensionDescriptor,
    policy: DappLaunchPolicy
  ): boolean {
    if (descriptor.status !== 'proposed') return true;
    return (
      (descriptor.cip === 104 || descriptor.cip === 142) &&
      policy.extensionRevision(descriptor.cip) >=
        (descriptor.policyRevision ?? Number.MAX_SAFE_INTEGER)
    );
  }

  private deviceRefusalReasons(
    cip: number | undefined,
    context: CapabilityContext
  ): string[] {
    if (context.walletKind === 'shelley-software') return [];
    const reasons: string[] = [];
    const { device } = context;
    if (!device) return ['missing-device-evidence'];
    if (device.vendor !== context.walletKind)
      reasons.push('wallet-kind-mismatch');
    if (!hasText(device.matrixRevision))
      reasons.push('missing-matrix-revision');
    else if (device.matrixRevision !== HARDWARE_CONNECTOR_MATRIX_REVISION)
      reasons.push('matrix-revision-mismatch');
    if (!hasText(device.rowId)) reasons.push('missing-row-id');
    if (!hasText(device.model)) reasons.push('missing-model');

    let version: string | undefined;
    if (device.vendor === 'ledger') {
      version = device.appVersion;
      if (!hasText(version || '')) reasons.push('missing-ledger-app-version');
      if (device.firmwareVersion !== undefined)
        reasons.push('unexpected-ledger-firmware-version');
    } else {
      version = device.firmwareVersion;
      if (!hasText(version || ''))
        reasons.push('missing-trezor-firmware-version');
      if (device.appVersion !== undefined)
        reasons.push('unexpected-trezor-app-version');
    }
    if (
      device.rowId !==
      hardwareConnectorRowId(device.vendor, device.model, version || '')
    )
      reasons.push('row-id-mismatch');
    if (!device.physicalCertified) reasons.push('unsupported-hardware-version');
    if (!device.packagedEnabled) reasons.push('packaged-policy-disabled');
    if (cip !== undefined && !device.certifiedExtensions.includes(cip))
      reasons.push('extension-not-certified');
    return reasons;
  }
}
