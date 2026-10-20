/**
 * @jest-environment node
 */
import crypto from 'crypto';
import sass from 'sass';

describe('react-polymorph theme compatibility', () => {
  it('preserves the CSS produced by the original theme with Sass 1.44.0', () => {
    const result = sass.renderSync({
      file: require.resolve('react-polymorph/lib/themes/simple/SimpleOptions.scss'),
      logger: { warn() {}, debug() {} },
    });
    // Baseline: unpatched react-polymorph 1.0.4 compiled with the master's
    // Sass 1.44.0. Ignore whitespace so OS line endings do not affect the check.
    const css = result.css.toString().replace(/\s+/g, '');
    expect(crypto.createHash('sha256').update(css).digest('hex')).toBe(
      '155bf3f9bedb6232f9fa8cce636caa32ae8091020951e521f04c1bae5742c0f7'
    );
  });
});
