const gulp = require('gulp');
const webpack = require('webpack');
const webpackStream = require('webpack-stream');
const shell = require('gulp-shell');
const electronConnect = require('electron-connect');
const mainWebpackConfig = require('./source/main/webpack.config');
const rendererWebpackConfig = require('./source/renderer/webpack.config');

// Setup electron-connect server to start the app in development mode
let electronServer;
// Gulp input sources for main and renderer compilation
const mainInputSource = () => gulp.src('source/main/index.ts');
const rendererInputSource = () => gulp.src('source/renderer/index.ts');
// Webpack watch configs
const mainWebpackWatchConfig = Object.assign({}, mainWebpackConfig, {
  watch: true,
});
const rendererWebpackWatchConfig = Object.assign({}, rendererWebpackConfig, {
  watch: true,
});
// Gulp output destinations for main and renderer compilation
const mainOutputDestination = () => gulp.dest('dist/main');
const rendererOutputDestination = () => gulp.dest('dist/renderer');

/**
 * Creates an electron-connect server instance that enables
 * us to control our app (restarting / reloading)
 * @param env - electron app environment
 * @param args - additional spawn options
 */
const createElectronServer = (env, args = []) => {
  electronServer = electronConnect.server.create({
    spawnOpt: {
      env: Object.assign({}, process.env, env),
      args,
    },
  });
};

const buildMain = () => () =>
  mainInputSource()
    .pipe(webpackStream(mainWebpackConfig, webpack))
    .pipe(mainOutputDestination());

const buildMainWatch = () => (done) =>
  mainInputSource()
    .pipe(
      webpackStream(mainWebpackWatchConfig, webpack, () => {
        // Restart app every time after main script has been re-compiled
        electronServer.restart();
        done();
      })
    )
    .pipe(mainOutputDestination());

const buildRenderer = () => () =>
  rendererInputSource()
    .pipe(webpackStream(rendererWebpackConfig, webpack))
    .pipe(rendererOutputDestination());

const buildRendererWatch = () => (done) =>
  rendererInputSource()
    .pipe(
      webpackStream(rendererWebpackWatchConfig, webpack, () => {
        if (electronServer) {
          // Reload app every time after renderer script has been re-compiled
          electronServer.reload();
        }
        done();
      })
    )
    .pipe(rendererOutputDestination());

gulp.task(
  'clear:cache',
  shell.task('rimraf ./node_modules/.cache && rimraf .cache-loader')
);

gulp.task('clean:dist', shell.task('rimraf ./dist'));

gulp.task('server:start', (done) => {
  electronServer.start();
  done();
});

gulp.task('server:create:dev', (done) => {
  createElectronServer({
    NODE_ENV: process.env.NODE_ENV,
    XCURSOR_PATH: '/usr/share/icons' || 'development',
  });
  done();
});

gulp.task('server:create:debug', (done) => {
  createElectronServer(
    {
      NODE_ENV: process.env.NODE_ENV,
      XCURSOR_PATH: '/usr/share/icons' || 'development',
    },
    ['--inspect', '--inspect-brk']
  );
  done();
});

gulp.task('build:main', buildMain());

gulp.task('build:main:watch', buildMainWatch());

gulp.task('build:renderer:html', () =>
  gulp.src('source/renderer/index.html').pipe(gulp.dest('dist/renderer/'))
);

gulp.task('build:renderer:assets', buildRenderer());

gulp.task(
  'build:renderer',
  gulp.series('build:renderer:html', 'build:renderer:assets')
);

gulp.task('build:renderer:watch', buildRendererWatch());

gulp.task('build', gulp.series('clean:dist', 'build:main', 'build:renderer'));

gulp.task('prepare:themes:utils', () =>
  gulp
    .src([
      'source/renderer/app/themes/utils/checkCreateTheme.ts',
      'source/renderer/app/themes/utils/constants.ts',
      'source/renderer/app/themes/utils/createShades.ts',
      'source/renderer/app/themes/utils/createTheme.ts',
      'source/renderer/app/themes/utils/findUpdates.ts',
      'source/renderer/app/themes/utils/updateThemes.ts',
      'source/renderer/app/themes/utils/updateThemesCLI.ts',
      'source/renderer/app/themes/utils/writeThemeUpdate.ts',
    ])
    .pipe(gulp.dest('dist/utils'))
);

gulp.task('prepare:themes:daedalus', () =>
  gulp
    .src([
      'source/renderer/app/themes/daedalus/cardano.ts',
      'source/renderer/app/themes/daedalus/dark-blue.ts',
      'source/renderer/app/themes/daedalus/dark-cardano.ts',
      'source/renderer/app/themes/daedalus/flight-candidate.ts',
      'source/renderer/app/themes/daedalus/incentivized-testnet.ts',
      'source/renderer/app/themes/daedalus/index.ts',
      'source/renderer/app/themes/daedalus/light-blue.ts',
      'source/renderer/app/themes/daedalus/shelley-testnet.ts',
      'source/renderer/app/themes/daedalus/white.ts',
      'source/renderer/app/themes/daedalus/yellow.ts',
    ])
    .pipe(gulp.dest('dist/daedalus'))
);

gulp.task('prepare:themes:scripts', () =>
  gulp
    .src([
      'source/renderer/app/themes/scripts/check.ts',
      'source/renderer/app/themes/scripts/update.ts',
    ])
    .pipe(gulp.dest('dist/scripts'))
);

gulp.task(
  'prepare:themes',
  gulp.series(
    'prepare:themes:utils',
    'prepare:themes:daedalus',
    'prepare:themes:scripts'
  )
);

gulp.task(
  'build:watch',
  gulp.series(
    'clean:dist',
    'server:create:dev',
    'build:renderer:html',
    'build:main:watch',
    'build:renderer:watch'
  )
);

gulp.task('build:themes', gulp.series('clean:dist', 'prepare:themes'));

gulp.task(
  'test:e2e:nodemon',
  shell.task(
    'nodemon --watch dist --watch tests --exec "yarn test:e2e --tags \'@e2e and @watch\'"'
  )
);

gulp.task('test:e2e:watch', gulp.series('build:watch', 'test:e2e:nodemon'));

gulp.task('purge:translations', shell.task('rimraf ./translations/messages'));

gulp.task('electron:inspector', shell.task('yarn electron:inspector'));

gulp.task(
  'start',
  shell.task(
    `cross-env NODE_ENV=${process.env.NODE_ENV || 'production'} electron ./`
  )
);

gulp.task(
  'dev',
  gulp.series('server:create:dev', 'build:watch', 'server:start')
);

gulp.task(
  'debug',
  gulp.series(
    'server:create:debug',
    'build:watch',
    'server:start',
    'electron:inspector'
  )
);
