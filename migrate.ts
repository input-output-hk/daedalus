import path from 'path';
import { tsIgnorePlugin, eslintFixPlugin } from 'ts-migrate-plugins';
import { migrate, MigrateConfig } from 'ts-migrate-server';

// get input files folder
const inputDir = path.resolve(__dirname);

// create new migration config and add ts-ignore plugin with options
const config = new MigrateConfig()
  .addPlugin(eslintFixPlugin, {})
  .addPlugin(tsIgnorePlugin, {
    useTsIgnore: true,
  });
// run migration
(async () => {
  const exitCode = await migrate({
    rootDir: inputDir,
    tsConfigDir: path.resolve(__dirname),
    config,
    sources: [
      './scripts/**/*.ts{,x}',
      './source/**/*.ts{,x}',
      './storybook/**/*.ts{,x}',
      './tests/**/*.ts{,x}',
      './translations/**/*.ts{,x}',
      './utils/**/*.ts{,x}',
      './declarations.d.ts',
    ],
  });
  process.exit(exitCode);
})();
