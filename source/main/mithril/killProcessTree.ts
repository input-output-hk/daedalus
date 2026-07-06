import { exec, execSync } from 'child_process';
import type { ChildProcess } from 'child_process';
import { logger } from '../utils/logging';
import { environment } from '../environment';

const TASKKILL_TIMEOUT_MS = 5000;

/**
 * Kill a runner-spawned mithril child and its descendants.
 * POSIX: process.kill(-pid) group-kills the tree — relies on the runner's detached spawn making the child a group leader.
 * Windows: taskkill /t /f — async by default so interactive kill sites never block the main thread;
 *   sync when opts.sync, because the shutdown reap runs as safeExitWithCode reaches process.exit() inside
 *   a stream-end callback, where an async taskkill would never launch.
 * signal is a no-op on Windows (taskkill is always a hard kill). Null/missing pid no-ops; a failed group kill
 *   falls back to child.kill() and never throws.
 */
export function killProcessTree(
  child: ChildProcess | null,
  signal: NodeJS.Signals = 'SIGTERM',
  opts: { sync?: boolean } = {}
): void {
  if (!child || child.pid == null) return;
  const { pid } = child;

  const fallbackDirectKill = (error: unknown) => {
    logger.warn(
      'killProcessTree: group kill failed; falling back to direct child.kill',
      { pid, signal, error }
    );
    try {
      child.kill(signal);
    } catch (fallbackError) {
      logger.warn('killProcessTree: direct child.kill fallback failed', {
        pid,
        signal,
        error: fallbackError,
      });
    }
  };

  try {
    if (environment.isWindows) {
      const command = `taskkill /pid ${pid} /t /f`;
      if (opts.sync === true) {
        execSync(command, {
          stdio: 'ignore',
          timeout: TASKKILL_TIMEOUT_MS,
          windowsHide: true,
        });
      } else {
        exec(
          command,
          { windowsHide: true, timeout: TASKKILL_TIMEOUT_MS },
          (error) => {
            if (error) fallbackDirectKill(error);
          }
        );
      }
    } else {
      process.kill(-pid, signal);
    }
  } catch (error) {
    fallbackDirectKill(error);
  }
}
