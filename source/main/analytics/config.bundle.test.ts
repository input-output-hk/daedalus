/** @jest-environment node */
import fs from 'fs';
import os from 'os';
import path from 'path';
import vm from 'vm';
import webpack from 'webpack';

// Compile the real configuration module using the real main bundle's plugins.
// Executing this tiny bundle never imports Electron or starts the application.
test('the main webpack EnvironmentPlugin freezes the packaged destination', async () => {
  const original = process.env.DAEDALUS_ARIADNE_ANALYTICS_URL;
  const directory = fs.mkdtempSync(
    path.join(os.tmpdir(), 'ariadne-config-test-')
  );
  const signed = 'https://signed.example.invalid/api/analytics/event';
  try {
    process.env.DAEDALUS_ARIADNE_ANALYTICS_URL = signed;
    const config = require('../webpack.config');
    await new Promise<void>((resolve, reject) => {
      const compiler = webpack({
        ...config,
        entry: path.resolve(__dirname, 'config.ts'),
        output: {
          path: directory,
          filename: 'config.cjs',
          library: { type: 'commonjs2' },
        },
        devtool: false,
      });
      compiler.run((error, stats) => {
        compiler.close((closeError) => {
          if (error || closeError || stats.hasErrors())
            reject(error || closeError || new Error(stats.toString()));
          else resolve();
        });
      });
    });
    const code = fs.readFileSync(path.join(directory, 'config.cjs'), 'utf8');
    const compiled = {
      exports: {} as {
        analyticsConfig: (
          env: Record<string, string>,
          packaged: boolean
        ) => unknown;
      },
    };
    const runtime = {
      DAEDALUS_ARIADNE_ANALYTICS_ENABLED: 'true',
      DAEDALUS_ARIADNE_ANALYTICS_URL:
        'https://runtime.example.invalid/api/analytics/event',
    };
    vm.runInNewContext(code, {
      module: compiled,
      URL,
      process: { env: runtime },
    });
    expect(compiled.exports.analyticsConfig(runtime, true)).toEqual({
      endpoint: signed,
    });
    expect(compiled.exports.analyticsConfig(runtime, false)).toEqual({
      endpoint: runtime.DAEDALUS_ARIADNE_ANALYTICS_URL,
    });
  } finally {
    if (original === undefined)
      delete process.env.DAEDALUS_ARIADNE_ANALYTICS_URL;
    else process.env.DAEDALUS_ARIADNE_ANALYTICS_URL = original;
    // Only remove the freshly allocated test directory beneath the OS temp root.
    if (
      path.dirname(directory) === path.resolve(os.tmpdir()) &&
      path.basename(directory).startsWith('ariadne-config-test-')
    )
      fs.rmSync(directory, { recursive: true, force: true });
  }
}, 30000);
