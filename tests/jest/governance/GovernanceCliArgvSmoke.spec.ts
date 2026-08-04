import { spawnSync } from 'child_process';

/**
 * Parse-only smoke test against the real cardano-cli binary: proves the exact
 * argv the service builds clears the CLI argument parser. The mocked unit
 * suite can only confirm the argv matches the developer's belief about the
 * grammar; the prepend-vs-append network-flag regression class is only
 * closable against the real parser. No socket is provided, so a passing
 * invocation dies at the env-var/connection stage — never in the parser.
 */

const CLI_BIN = 'cardano-cli';

/** Mirrors GovernanceQueryService.CLI_USAGE_SIGNATURE. */
const USAGE_SIGNATURE = /(invalid (option|argument)|missing:|usage:)/i;

const isCliOnPath = (() => {
  try {
    return spawnSync(CLI_BIN, ['--version'], { timeout: 10_000 }).status === 0;
  } catch {
    return false;
  }
})();

// Self-skip (never fail) where the real binary is absent, so the mocked CI
// unit lane and plain devcontainers stay green; the positive run needs the
// Nix shell with the bundled cardano-cli on PATH.
const describeWithCli = isCliOnPath ? describe : describe.skip;

/**
 * The exact per-phase argv forms GovernanceQueryService builds: era token
 * first, subcommand args, network flag appended last.
 */
const SERVICE_ARGV_FORMS: string[][] = [];
(['latest', 'conway'] as const).forEach((era) => {
  [
    ['query', 'drep-state', '--all-dreps', '--output-json'],
    ['query', 'tip', '--output-json'],
    ['query', 'drep-stake-distribution', '--all-dreps', '--output-json'],
  ].forEach((args) => {
    SERVICE_ARGV_FORMS.push([era, ...args, '--mainnet']);
    SERVICE_ARGV_FORMS.push([era, ...args, '--testnet-magic', '1']);
  });
});

describeWithCli('cardano-cli argv smoke (parse-only, no socket)', () => {
  it.each(
    SERVICE_ARGV_FORMS.map((argv): [string, string[]] => [argv.join(' '), argv])
  )('clears the argument parser: %s', (_label, argv) => {
    const env: Record<string, string> = {};
    Object.keys(process.env).forEach((key) => {
      const value = process.env[key];
      if (typeof value === 'string' && key !== 'CARDANO_NODE_SOCKET_PATH') {
        env[key] = value;
      }
    });

    const result = spawnSync(CLI_BIN, argv, {
      env: env as typeof process.env,
      timeout: 15_000,
    });
    const stderr = String(result.stderr ?? '');

    // Without a socket the query cannot succeed; it must fail at the
    // socket/connection stage. An optparse rejection (Invalid option /
    // "Missing: (--mainnet | --testnet-magic NATURAL)" / unknown era) is
    // the grammar regression this test exists to catch.
    expect(result.status).not.toBe(0);
    expect(stderr).not.toMatch(USAGE_SIGNATURE);
  });
});
