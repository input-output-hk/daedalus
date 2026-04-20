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

const getWindowsPathKey = (env: NodeJS.ProcessEnv): string => {
  const pathKeys = Object.keys(env).filter(
    (key) => key.toLowerCase() === 'path'
  );

  if (pathKeys.includes('Path')) return 'Path';
  if (pathKeys.includes('PATH')) return 'PATH';
  return pathKeys[0] || 'Path';
};

const dedupeWindowsPathSegments = (segments: Array<string>): Array<string> => {
  const seen = new Set<string>();

  return segments.filter((segment) => {
    const normalized = segment.trim();
    if (!normalized) return false;

    const fingerprint = normalized.toLowerCase();
    if (seen.has(fingerprint)) return false;

    seen.add(fingerprint);
    return true;
  });
};

export function normalizeSpawnEnv(env: NodeJS.ProcessEnv): NodeJS.ProcessEnv {
  if (!environment.isWindows) return env;

  const nextEnv = { ...env };
  const pathKey = getWindowsPathKey(nextEnv);
  const pathDelimiter = ';';
  const pathValues = Object.keys(nextEnv)
    .filter((key) => key.toLowerCase() === 'path')
    .map((key) => nextEnv[key])
    .filter(Boolean);
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY;
  const currentPath =
    pathValues.join(pathDelimiter) ||
    process.env[pathKey] ||
    process.env.Path ||
    process.env.PATH ||
    '';

  Object.keys(nextEnv)
    .filter((key) => key.toLowerCase() === 'path' && key !== pathKey)
    .forEach((key) => delete nextEnv[key]);

  const normalizedPath = dedupeWindowsPathSegments(
    [installDir, currentPath]
      .filter(Boolean)
      .reduce<
        Array<string>
      >((segments, value) => segments.concat(String(value).split(pathDelimiter)), [])
  ).join(pathDelimiter);

  if (normalizedPath) {
    nextEnv[pathKey] = normalizedPath;
  }

  if (!nextEnv.SystemRoot && process.env.SystemRoot) {
    nextEnv.SystemRoot = process.env.SystemRoot;
  }

  if (!nextEnv.ComSpec && process.env.ComSpec) {
    nextEnv.ComSpec = process.env.ComSpec;
  }

  return nextEnv;
}

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

  const env = normalizeSpawnEnv(await buildMithrilEnv(requireKeys));
  const pathKey = getWindowsPathKey(env);

  // Resolve mithril-client binary path
  const binaryName = environment.isWindows
    ? 'mithril-client.exe'
    : 'mithril-client';
  const installDir = process.env.DAEDALUS_INSTALL_DIRECTORY;
  const binaryPath = installDir
    ? path.join(installDir, binaryName)
    : binaryName;
  const commandArgs = ['--origin-tag', 'DAEDALUS', ...args];

  ensureDirectoryExists(workDir);

  logger.info(`[mithril] Spawning: ${binaryPath} ${commandArgs.join(' ')}`, {
    binaryPath,
    installDir: installDir || '(not set)',
    cwd: workDir,
    pathEnv: env[pathKey] || '(not set)',
    pathKey,
  });

  return new Promise((resolve, reject) => {
    const child = spawn(binaryPath, commandArgs, {
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
