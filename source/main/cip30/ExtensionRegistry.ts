import { hasDappCip30MethodSchema } from '../../common/cip30/schemas';
import { DappCip30Method } from '../../common/cip30/wire';
import {
  BASE_DESCRIPTOR,
  BaseDescriptor,
  EXTENSION_DESCRIPTORS,
  ExtensionDescriptor,
  MethodDescriptor,
} from './extensions';

export type ResolvedMethod = Readonly<{
  descriptor: MethodDescriptor;
  extension?: number;
  override?: number;
}>;

type SchemaLookup = (method: DappCip30Method) => boolean;

const assertMethods = (
  methods: readonly MethodDescriptor[],
  hasSchema: SchemaLookup
): void => {
  methods.forEach((method) => {
    if (!Array.isArray(method.scopes) || !hasSchema(method.path)) {
      throw new Error(`Invalid CIP-30 method descriptor: ${method.path}`);
    }
  });
};

export class ExtensionRegistry {
  readonly base: BaseDescriptor;
  readonly descriptors: readonly ExtensionDescriptor[];
  private readonly byCip = new Map<number, ExtensionDescriptor>();
  private readonly methodOwners = new Map<
    DappCip30Method,
    ExtensionDescriptor
  >();

  constructor(
    base: BaseDescriptor = BASE_DESCRIPTOR,
    descriptors: readonly ExtensionDescriptor[] = EXTENSION_DESCRIPTORS,
    hasSchema: SchemaLookup = hasDappCip30MethodSchema
  ) {
    this.base = base;
    this.descriptors = descriptors;
    assertMethods(base.methods, hasSchema);

    const baseMethods = new Set(base.methods.map(({ path }) => path));
    const namespaces = new Set<string>();
    const overridePaths = new Set<DappCip30Method>();
    descriptors.forEach((descriptor) => {
      if (
        this.byCip.has(descriptor.cip) ||
        namespaces.has(descriptor.namespace) ||
        !Array.isArray(descriptor.scopes)
      ) {
        throw new Error(
          `Duplicate or invalid CIP-${descriptor.cip} descriptor`
        );
      }
      this.byCip.set(descriptor.cip, descriptor);
      namespaces.add(descriptor.namespace);
      assertMethods(descriptor.methods, hasSchema);
      assertMethods(descriptor.baseOverrides, hasSchema);

      descriptor.methods.forEach((method) => {
        if (
          baseMethods.has(method.path) ||
          this.methodOwners.has(method.path)
        ) {
          throw new Error(`Duplicate CIP-30 method: ${method.path}`);
        }
        this.methodOwners.set(method.path, descriptor);
      });
      descriptor.baseOverrides.forEach((method) => {
        if (!baseMethods.has(method.path)) {
          throw new Error(`Undeclared base override: ${method.path}`);
        }
        if (overridePaths.has(method.path)) {
          throw new Error(`Duplicate base override: ${method.path}`);
        }
        overridePaths.add(method.path);
      });
    });

    descriptors.forEach(({ cip, dependencies }) =>
      dependencies.forEach((dependency) => {
        if (!this.byCip.has(dependency)) {
          throw new Error(
            `Unknown CIP-${dependency} dependency for CIP-${cip}`
          );
        }
      })
    );
    this.assertAcyclic();
  }

  isKnown(cip: number): boolean {
    return this.byCip.has(cip);
  }

  get(cip: number): ExtensionDescriptor | undefined {
    return this.byCip.get(cip);
  }

  resolve(
    method: DappCip30Method,
    enabledExtensions: readonly number[]
  ): ResolvedMethod | undefined {
    const owner = this.methodOwners.get(method);
    if (owner) {
      return enabledExtensions.includes(owner.cip)
        ? {
            descriptor: owner.methods.find(({ path }) => path === method)!,
            extension: owner.cip,
          }
        : undefined;
    }

    const base = this.base.methods.find(({ path }) => path === method);
    if (!base) return undefined;
    for (const descriptor of this.descriptors) {
      if (!enabledExtensions.includes(descriptor.cip)) continue;
      const override = descriptor.baseOverrides.find(
        ({ path }) => path === method
      );
      if (override) {
        return { descriptor: override, override: descriptor.cip };
      }
    }
    return { descriptor: base };
  }

  compositionTarget(
    method: DappCip30Method,
    enabledExtensions: readonly number[]
  ): ResolvedMethod | undefined {
    const resolved = this.resolve(method, enabledExtensions);
    return resolved?.descriptor.composesWith
      ? this.resolve(resolved.descriptor.composesWith, enabledExtensions)
      : undefined;
  }

  private assertAcyclic(): void {
    const visiting = new Set<number>();
    const visited = new Set<number>();
    const visit = (cip: number): void => {
      if (visiting.has(cip)) throw new Error('CIP extension dependency cycle');
      if (visited.has(cip)) return;
      visiting.add(cip);
      this.byCip.get(cip)!.dependencies.forEach(visit);
      visiting.delete(cip);
      visited.add(cip);
    };
    this.descriptors.forEach(({ cip }) => visit(cip));
  }
}
