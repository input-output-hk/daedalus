import { Environment } from '../common/types/environment.types';

declare module '*.scss';
declare module '*.inline.svg' {
  const content: React.FC<React.SVGProps<SVGSVGElement>>;
  export default content;
}
declare namespace globalThis {
  /* eslint-disable-next-line */
  var isFlight: boolean;
  /* eslint-disable-next-line */
  var environment: Environment;
}
