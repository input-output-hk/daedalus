import { exec, execSync } from 'child_process';
import type { ChildProcess } from 'child_process';
import { logger } from '../utils/logging';
import { environment } from '../environment';

const TASKKILL_TIMEOUT_MS = 5000;

/**
 * Defense-in-depth: kill a runner-spawned mithril child together with any
 * descendants it may have forked.
 *
 * - POSIX (both modes): `process.kill(-pid, signal)` — a synchronous process-GROUP kill.
 *   It relies on the shared runner spawning with `detached: !environment.isWindows`, which
 *   makes the child a group leader, so `-pid` names its whole tree.
 * - Windows, default async (interactive kill sites — cancel()/forceKill()/late-kill):
 *   non-blocking `exec('taskkill /pid <pid> /t /f')` so the main thread (and UI) never
 *   freezes for up to 5 s; the callback logs a failure and falls back to a direct
 *   `child.kill()`.
 * - Windows, `opts.sync: true` (the shutdown reap ONLY): `execSync` of the same command —
 *   `safeExitWithCode` reaches `process.exit()` inside a stream-end callback, so an async
 *   taskkill issued that late would never actually launch.
 *
 * `taskkill /f` is always a hard kill, so the `signal` argument is a documented no-op on
 * Windows (the graceful SIGTERM stage collapses into a force kill there, matching
 * CardanoNode's Windows behavior). A null child or missing pid is a no-op. Any throw from
 * the group kill falls back to `child.kill(signal)`; a throwing fallback is swallowed with
 * a warn — this helper must never propagate during cancel or shutdown.
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
