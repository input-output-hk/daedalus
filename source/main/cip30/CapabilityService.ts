import { ApiError } from '../../common/cip30/errors';
import { DappCip30Method } from '../../common/cip30/wire';
import { DappLaunchPolicy } from '../dapp/DappLaunchPolicy';
import { ExtensionRegistry, ResolvedMethod } from './ExtensionRegistry';
import { ExtensionDescriptor } from './extensions';

export type DappWalletKind = 'shelley-software' | 'ledger' | 'trezor' | 'byron';

export type HardwareCapabilityEvidence = Readonly<{
  deviceType: 'ledger' | 'trezor';
  model: string;
  appVersion: string;
  firmwareVersion: string;
  supportedExtensions: readonly number[];
}>;

export type CapabilityContext = Readonly<{
  walletKind: DappWalletKind;
  backendApiVersion: number;
  backendExtensions: readonly number[];
  networkSupported: boolean;
  device?: HardwareCapabilityEvidence;
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
    return !descriptor.requiresDevice || this.deviceAllows(cip, context);
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

  private deviceAllows(cip: number, context: CapabilityContext): boolean {
    if (context.walletKind === 'shelley-software') return true;
    const device = context.device;
    return (
      device !== undefined &&
      device.deviceType === context.walletKind &&
      hasText(device.model) &&
      hasText(device.appVersion) &&
      hasText(device.firmwareVersion) &&
      device.supportedExtensions.includes(cip)
    );
  }
}
