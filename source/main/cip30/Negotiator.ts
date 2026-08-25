import { parseDappCip30GatewayRequest } from '../../common/cip30/schemas';
import { EnableOptions, Extension } from '../../common/cip30/wire';
import { CapabilityContext, CapabilityService } from './CapabilityService';
import { ExtensionRegistry } from './ExtensionRegistry';
import { ExtensionDescriptor } from './extensions';

export type NegotiationResult = Readonly<{
  supportedExtensions: readonly Extension[];
  enabledExtensions: readonly Extension[];
}>;

export class Negotiator {
  constructor(
    private readonly registry: ExtensionRegistry,
    private readonly capabilities: CapabilityService
  ) {}

  supported(context: CapabilityContext): readonly Extension[] {
    const supported = new Map<number, boolean>();
    const visit = (descriptor: ExtensionDescriptor): boolean => {
      const cached = supported.get(descriptor.cip);
      if (cached !== undefined) return cached;
      const value =
        this.capabilities.isSupported(descriptor.cip, context) &&
        descriptor.dependencies.every((cip) => {
          const dependency = this.registry.get(cip);
          return dependency !== undefined && visit(dependency);
        });
      supported.set(descriptor.cip, value);
      return value;
    };
    return this.registry.descriptors
      .filter(visit)
      .map(({ cip }) => Object.freeze({ cip }));
  }

  negotiate(options: unknown, context: CapabilityContext): NegotiationResult {
    const request = parseDappCip30GatewayRequest({
      method: 'provider.enable',
      args: options === undefined ? [] : [options],
    });
    const requested = new Set(
      ((request.args[0] as EnableOptions | undefined)?.extensions ?? []).map(
        ({ cip }) => cip
      )
    );
    const supportedExtensions = this.supported(context);
    const supported = new Set(supportedExtensions.map(({ cip }) => cip));
    const enabled: number[] = [];

    this.registry.descriptors.forEach((descriptor) => {
      if (
        !requested.has(descriptor.cip) ||
        !supported.has(descriptor.cip) ||
        !descriptor.dependencies.every((cip) => enabled.includes(cip)) ||
        descriptor.incompatibleWith.some((cip) => enabled.includes(cip))
      ) {
        return;
      }
      enabled.push(descriptor.cip);
    });

    return Object.freeze({
      supportedExtensions,
      enabledExtensions: enabled.map((cip) => Object.freeze({ cip })),
    });
  }
}
