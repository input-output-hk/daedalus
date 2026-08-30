export const DAPP_POLICY_REVISION = 1;
export const TASK_607_CERTIFIED_HARDWARE_ROWS: readonly string[] = Object.freeze(
  []
);

export type DappLaunchMode = 'preferred' | 'diagnostics';

export type DappLaunchPolicyConfig = Readonly<{
  revision: typeof DAPP_POLICY_REVISION;
  globalEnabled: boolean;
  preferredCatalogEnabled: boolean;
  diagnosticsEnabled: boolean;
  cip104Revision: number;
  cip142Revision: number;
  hardwareConnectorRows: readonly string[];
}>;

const DISABLED_POLICY: DappLaunchPolicyConfig = Object.freeze({
  revision: DAPP_POLICY_REVISION,
  globalEnabled: false,
  preferredCatalogEnabled: false,
  diagnosticsEnabled: false,
  cip104Revision: 0,
  cip142Revision: 0,
  hardwareConnectorRows: Object.freeze([]),
});

const isPlainObject = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === 'object' && !Array.isArray(value);

const isRevision = (value: unknown): value is number =>
  Number.isSafeInteger(value) && Number(value) >= 0;

const hardwareRows = (
  value: unknown,
  certifiedRows: readonly string[]
): readonly string[] => {
  if (
    value === undefined ||
    (Array.isArray(value) &&
      new Set(value).size === value.length &&
      value.every(
        (row) => typeof row === 'string' && certifiedRows.includes(row)
      ))
  )
    return Object.freeze(Array.isArray(value) ? [...value] : []);
  return Object.freeze([]);
};

export class DappLaunchPolicy {
  readonly config: DappLaunchPolicyConfig;

  constructor(
    value: unknown,
    certifiedRows = TASK_607_CERTIFIED_HARDWARE_ROWS
  ) {
    if (
      !isPlainObject(value) ||
      value.revision !== DAPP_POLICY_REVISION ||
      typeof value.globalEnabled !== 'boolean' ||
      typeof value.preferredCatalogEnabled !== 'boolean' ||
      typeof value.diagnosticsEnabled !== 'boolean' ||
      !isRevision(value.cip104Revision) ||
      !isRevision(value.cip142Revision)
    ) {
      this.config = DISABLED_POLICY;
      return;
    }

    this.config = Object.freeze({
      revision: DAPP_POLICY_REVISION,
      globalEnabled: value.globalEnabled,
      preferredCatalogEnabled: value.preferredCatalogEnabled,
      diagnosticsEnabled: value.diagnosticsEnabled,
      cip104Revision: value.cip104Revision,
      cip142Revision: value.cip142Revision,
      hardwareConnectorRows: hardwareRows(
        value.hardwareConnectorRows,
        certifiedRows
      ),
    });
  }

  allows(mode: DappLaunchMode): boolean {
    return (
      this.config.globalEnabled &&
      (mode === 'preferred'
        ? this.config.preferredCatalogEnabled
        : this.config.diagnosticsEnabled)
    );
  }

  extensionRevision(cip: 104 | 142): number {
    return cip === 104
      ? this.config.cip104Revision
      : this.config.cip142Revision;
  }

  hardwareConnectorEnabled(rowId: string): boolean {
    return this.config.hardwareConnectorRows.includes(rowId);
  }
}
