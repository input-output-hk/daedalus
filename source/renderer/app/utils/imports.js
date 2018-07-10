// @flow
import environment from '../../../common/environment';

/**
 * Resolves environment specific overrides of a file in
 * the subfolders of a pre-defined webpack context.
 * This assumes that you provide the file name within the
 * webpack context filter regexplike /example.js/
 *
 * @param context - configured require.context instance
 * @returns {*}
 */
export const resolve = (context) => {
  // Extract the file name from the given context regexp
  const fileName = context.keys()[0].match(/.*\/(.*.js)/)[1];
  let file;
  try {
    file = context(`./${environment.API}/${fileName}`);
  } catch (e) {
    file = context(`./${fileName}`);
  }
  return file.default || file;
};
