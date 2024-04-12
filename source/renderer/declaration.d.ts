import { Environment } from '../common/types/environment.types';

declare module '*.scss';
declare module '*.svg' {
  import React = require('react');

  const SVG: React.FunctionComponent<React.SVGProps<SVGSVGElement>>;
  export default SVG;
}
declare namespace globalThis {
  /* eslint-disable-next-line */
  var isFlight: boolean;
  /* eslint-disable-next-line */
  var environment: Environment;
}
