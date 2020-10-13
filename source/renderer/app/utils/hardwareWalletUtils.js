// @flow
import { map } from 'lodash';
export const derivationPathToString = (
  path: Array<string>,
) => {
  let constructedPath = 'm';
  map(path, chunk => {
    constructedPath = `${constructedPath}/${chunk.replace('H', "'")}`;
  })
  return constructedPath;
};