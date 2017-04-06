import path from 'path';

export default (platform, env, appName) => {
  switch (platform) {
    case 'darwin': {
      return path.join(env['HOME'], 'Library', 'Application Support', appName, 'Logs');
    }
    case 'win32': {
      return path.join(env['APPDATA'], appName, 'Logs');
    }
  }
};
