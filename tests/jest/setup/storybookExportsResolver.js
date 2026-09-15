/*
 * Resolve Storybook's subpath exports for Jest 27.
 *
 * Storybook 8 publishes almost everything behind a package `exports` map:
 * `@storybook/preview-api` is a one-line shim for `storybook/internal/preview-api`,
 * which is a shim for `@storybook/core/preview-api`, and the bundle behind that
 * requires a dozen more `@storybook/core/*` subpaths. None of those paths exists
 * on disk under that name; each is a key in an `exports` map pointing somewhere
 * else. Jest 27 predates `exports` support, so it looks for the directory it was
 * asked for and reports the module as missing.
 *
 * Mapping the paths one at a time does not converge, because each resolved
 * bundle names more of them. This reads the map instead, which is what Node
 * does, and hands everything else back to Jest's own resolver.
 *
 * The CommonJS condition is chosen deliberately. The ESM entry beside it would
 * resolve, and then fail to parse: `transformIgnorePatterns` excludes
 * node_modules, so nothing would transpile it.
 */
const fs = require('fs');
const path = require('path');

const PACKAGES = ['@storybook/core', 'storybook'];

function packageRoot(name, basedir) {
  try {
    return path.dirname(
      require.resolve(`${name}/package.json`, {
        paths: [basedir, process.cwd()],
      })
    );
  } catch (e) {
    return null;
  }
}

// An exports value is a string, or a conditions object, possibly nested, or an
// array of alternatives. Take the first CommonJS-compatible string.
function pickRequireTarget(value) {
  if (typeof value === 'string') return value;
  if (Array.isArray(value)) {
    for (const entry of value) {
      const picked = pickRequireTarget(entry);
      if (picked) return picked;
    }
    return null;
  }
  if (value && typeof value === 'object') {
    for (const condition of ['require', 'node', 'default']) {
      if (condition in value) {
        const picked = pickRequireTarget(value[condition]);
        if (picked) return picked;
      }
    }
  }
  return null;
}

module.exports = function storybookExportsResolver(request, options) {
  const name = PACKAGES.find(
    (p) => request === p || request.startsWith(`${p}/`)
  );
  if (name) {
    const root = packageRoot(name, options.basedir);
    if (root) {
      const manifest = JSON.parse(
        fs.readFileSync(path.join(root, 'package.json'), 'utf8')
      );
      const subpath = request === name ? '.' : `.${request.slice(name.length)}`;
      const target = pickRequireTarget(
        manifest.exports && manifest.exports[subpath]
      );
      if (target) {
        const resolved = path.join(root, target);
        if (fs.existsSync(resolved)) return resolved;
      }
    }
  }
  return options.defaultResolver(request, options);
};
