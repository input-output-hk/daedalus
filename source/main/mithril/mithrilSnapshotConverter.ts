import path from 'path';
import fs from 'fs-extra';
import type { RunCommandResult } from './mithrilCommandRunner';

type ConvertSnapshotFailureFactory = (
  message: string,
  stderr?: string
) => Error;

type ConvertSnapshotDbToLsmOptions = {
  dbDirectory: string;
  configPath: string;
  runBinary(binaryName: string, args: string[]): Promise<RunCommandResult>;
  createFailure: ConvertSnapshotFailureFactory;
  onCommandPrepared?: (
    args: string[],
    context: { slot: string; dbDirectory: string }
  ) => void;
};

export async function convertSnapshotDbToLsm({
  dbDirectory,
  configPath,
  runBinary,
  createFailure,
  onCommandPrepared,
}: ConvertSnapshotDbToLsmOptions): Promise<{ slot: string }> {
  const ledgerDir = path.join(dbDirectory, 'ledger');
  const entries = await fs.readdir(ledgerDir, { withFileTypes: true });
  const slots = entries
    .filter((entry) => entry.isDirectory() && /^\d+$/.test(entry.name))
    .map((entry) => BigInt(entry.name))
    .sort((left, right) => {
      if (left < right) return -1;
      if (left > right) return 1;
      return 0;
    });

  if (slots.length === 0) {
    throw createFailure('No ledger snapshots found for conversion');
  }

  const slot = String(slots[slots.length - 1]);
  const inputMemPath = path.join(ledgerDir, slot);
  const tempInputPath = path.join(dbDirectory, slot);
  const outputLsmSnapshot = path.join(ledgerDir, slot);
  const outputLsmDatabase = path.join(dbDirectory, 'lsm');

  await fs.move(inputMemPath, tempInputPath);

  const converterArgs = [
    '--input-mem',
    tempInputPath,
    '--output-lsm-snapshot',
    outputLsmSnapshot,
    '--output-lsm-database',
    outputLsmDatabase,
    '--config',
    configPath,
  ];

  // Remove any existing lsm directory so snapshot-converter doesn't prompt.
  await fs.remove(outputLsmDatabase);
  onCommandPrepared?.(converterArgs, { slot, dbDirectory });

  let result: RunCommandResult;
  try {
    result = await runBinary('snapshot-converter', converterArgs);
  } finally {
    await fs.remove(tempInputPath).catch(() => {});
  }

  if (result.exitCode !== 0) {
    throw createFailure(
      `Snapshot conversion failed with exit code ${result.exitCode}`,
      result.stderr
    );
  }

  return { slot };
}
