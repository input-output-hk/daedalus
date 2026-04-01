import path from 'path';
import fs from 'fs-extra';
import { spawn } from 'child_process';
import type { ChildProcess } from 'child_process';
import type { WriteStream } from 'fs';
import ensureDirectoryExists from '../utils/ensureDirectoryExists';
import { stateDirectoryPath } from '../config';
import { buildMithrilEnv } from './mithrilNetworkConfig';
import { logger } from '../utils/logging';
import { environment } from '../environment';

export type RunCommandResult = {
  stdout: string;
  stderr: string;
  exitCode: number | null;
};

export type RunCommandOptions = {
  onStdout?: (chunk: string) => void;
  onStderr?: (chunk: string) => void;
  requireKeys?: boolean;
};

export type RunCommandCallbacks = {
  onProcess?: (child: ChildProcess | null) => void;
  onLogStream?: (logStream: WriteStream) => void;
};

export function openLogStream(): WriteStream {
  const logsDir = path.join(stateDirectoryPath, 'Logs');
  ensureDirectoryExists(logsDir);
  const logPath = path.join(logsDir, 'mithril-bootstrap.log');
  return fs.createWriteStream(logPath, { flags: 'a' });
}

export function attachLogStream(
  child: ChildProcess,
  logStream: WriteStream
): void {
  if (child.stdout) {
    child.stdout.on('data', (chunk) => logStream.write(chunk));
  }
  if (child.stderr) {
    child.stderr.on('data', (chunk) => logStream.write(chunk));
  }
}

export async function runCommand(
  args: string[],
  workDir: string,
  options: RunCommandOptions = {},
  callbacks?: RunCommandCallbacks
): Promise<RunCommandResult> {
  const { onStdout, onStderr, requireKeys = true } = options;
  const logStream = openLogStream();
  if (callbacks?.onLogStream) callbacks.onLogStream(logStream);

  const env = await buildMithrilEnv(requireKeys);

  // Resolve mithril-client binary path
  const binaryName = environment.isWindows
    ? 'mithril-client.exe'
    : 'mithril-client';
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY;
  const binaryPath = installDir
    ? path.join(installDir, binaryName)
    : binaryName;

  logger.info(`[mithril] Spawning: ${binaryPath} ${args.join(' ')}`, {
    binaryPath,
    installDir: installDir || '(not set)',
    cwd: workDir,
    pathEnv: env.PATH || '(not set)',
  });

  return new Promise((resolve, reject) => {
    const child = spawn(binaryPath, args, {
      cwd: workDir,
      env,
    });

    if (callbacks?.onProcess) callbacks.onProcess(child);
    attachLogStream(child, logStream);

    let stdout = '';
    let stderr = '';

    if (child.stdout) {
      child.stdout.on('data', (chunk) => {
        const text = chunk.toString();
        stdout += text;
        if (onStdout) onStdout(text);
      });
    }

    if (child.stderr) {
      child.stderr.on('data', (chunk) => {
        const text = chunk.toString();
        stderr += text;
        if (onStderr) onStderr(text);
      });
    }

    child.on('error', (error) => {
      if (callbacks?.onProcess) callbacks.onProcess(null);
      logStream.end();
      reject(error);
    });

    child.on('close', (exitCode) => {
      if (callbacks?.onProcess) callbacks.onProcess(null);
      logStream.end();
      resolve({ stdout, stderr, exitCode });
    });
  });
}
