import path from 'path';

const isProd = process.env.NODE_ENV === 'production';
export default (platform, env, appName) => {
  if (!isProd) {
    return "./";
  }
  switch (platform) {
    case 'darwin': {
      return path.join(env.HOME, 'Library', 'Application Support', appName);
    }
    case 'win32': {
      return path.join(env.APPDATA, appName);
    }
    case 'linux': {
      const { DAEDALUS_DIR, CLUSTER } = env;
      if (!!DAEDALUS_DIR && !!CLUSTER) {
        return DAEDALUS_DIR + '/' + CLUSTER;
      }
      return path.join(env.HOME, '.config', appName);
    }
    default: {
      console.log('Unsupported platform');
      process.exit(1);
    }
  }
};
