import path from 'path';

export default (platform, env, appName) => {
  switch (platform) {
    case 'darwin': {
      return path.join(env['HOME'], 'Library', 'Application Support', appName, 'Logs');
    }
    case 'win32': {
      return path.join(env['APPDATA'], appName, 'Logs');
    }
    case 'linux': {
      return path.join(env['HOME'], '.config', appName, 'Logs');
    }
    default: {
      console.log('Unsupported platform');
      process.exit(1);
    }
  }
};
