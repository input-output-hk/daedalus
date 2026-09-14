/**
 * Jest 27.5.1 predates `node:sqlite`. `jest-runtime` strips a `node:` prefix
 * and requires what is left, so `node:sqlite` becomes `require('sqlite')`,
 * which is not a module and has no unprefixed alias. `createRequire` does not
 * help either, because Jest substitutes its own `module`.
 *
 * `process.mainModule` inside the real context is Jest's own entry module, and
 * its `require` is the real Node one. Mapping `node:sqlite` here lets the
 * module under test keep the import it will run with under Electron.
 */
const vm = require('vm');

const realRequire = vm.runInThisContext(
  'process.mainModule && process.mainModule.require.bind(process.mainModule)'
);

if (!realRequire) {
  throw new Error(
    'node:sqlite shim: process.mainModule is unavailable, so the real Node require cannot be reached'
  );
}

module.exports = realRequire('node:sqlite');
