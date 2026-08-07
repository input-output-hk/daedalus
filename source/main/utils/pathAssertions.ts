// Helpers for writing path expectations in specs.
//
// The code under test builds paths with `path.join` and `path.resolve`. Both
// use the platform separator, and `path.resolve` additionally qualifies a path
// with the current drive on Windows, so a POSIX literal like
// '/tmp/state/chain' only matches on POSIX. Building the expectation the same
// way the code builds it keeps the assertion about the path rather than about
// the platform.
//
// Not a spec file: `testMatch` only picks up `*.spec.ts` / `*.test.ts`, so this
// is imported, never executed as a suite.

import path from 'path';

/**
 * A POSIX-written path, separated for the platform the test is running on.
 * Mirrors what `path.join` produces in the code under test.
 */
export const atPath = (posixPath: string): string =>
  posixPath.split('/').join(path.sep);

/**
 * As `atPath`, and additionally resolved against the working directory — which
 * on Windows means it gains a drive letter. Mirrors `path.resolve` in the code
 * under test; use it only where the code resolves, since the two differ.
 */
export const atResolvedPath = (posixPath: string): string =>
  path.resolve(atPath(posixPath));
