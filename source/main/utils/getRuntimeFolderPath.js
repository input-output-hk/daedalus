import path from 'path';

export default (platform, env, appName) => {
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
