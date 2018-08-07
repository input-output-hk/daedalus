const gulp = require('gulp');
const webpack = require('webpack');
const webpackStream = require('webpack-stream');
const mainWebpackConfig = require('./source/main/webpack.config');
const rendererWebpackConfig = require('./source/renderer/webpack.config');
const shell = require('gulp-shell');
const electronConnect = require('electron-connect');

// Setup electron-connect server to start the app in development mode
let electronServer;
// Gulp input sources for main and renderer compilation
const mainInputSource = () => gulp.src('source/main/index.js');
const rendererInputSource = () => gulp.src('source/renderer/index.js');
// Webpack watch configs
const mainWebpackWatchConfig = Object.assign({}, mainWebpackConfig, { watch: true });
const rendererWebpackWatchConfig = Object.assign({}, rendererWebpackConfig, { watch: true });
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
    }
  });
};

const buildMain = () => (
  () => (
    mainInputSource()
      .pipe(webpackStream(mainWebpackConfig, webpack))
      .pipe(mainOutputDestination())
  )
);

const buildMainWatch = () => (
  (done) => (
    mainInputSource()
      .pipe(webpackStream(mainWebpackWatchConfig, webpack, () => {
        // Restart app everytime after main script has been re-compiled
        electronServer.restart();
        done();
      }))
      .pipe(mainOutputDestination())
  )
);

const buildRenderer = () => (
  () => (
    rendererInputSource()
      .pipe(webpackStream(rendererWebpackConfig, webpack))
      .pipe(rendererOutputDestination())
  )
);

const buildRendererWatch = () => (
  (done) => (
    rendererInputSource()
      .pipe(webpackStream(rendererWebpackWatchConfig, webpack, () => {
        // Reload app everytime after renderer script has been re-compiled
        electronServer.reload();
        done();
      }))
      .pipe(rendererOutputDestination())
  )
);

gulp.task('clear:cache', shell.task('rimraf ./node_modules/.cache'));

gulp.task('clean:dist', shell.task('rimraf ./dist'));

gulp.task('server:start', (done) => {
  electronServer.start();
  done();
});

gulp.task('server:create:dev', (done) => {
  createElectronServer({ NODE_ENV: process.env.NODE_ENV || 'development' });
  done();
});

gulp.task('server:create:debug', (done) => {
  createElectronServer({ NODE_ENV: 'development' }, ['--inspect', '--inspect-brk']);
  done();
});

gulp.task('build:main', buildMain());

gulp.task('build:main:watch', buildMainWatch());

gulp.task('build:renderer:html', () => (
  gulp.src('source/renderer/index.html').pipe(gulp.dest('dist/renderer/'))
));

gulp.task('build:renderer:assets', buildRenderer());

gulp.task('build:renderer', gulp.series('build:renderer:html', 'build:renderer:assets'));

gulp.task('build:renderer:watch', buildRendererWatch());

gulp.task('build', gulp.series('clean:dist', 'build:main', 'build:renderer'));

gulp.task('build:watch', gulp.series(
  'clean:dist', 'server:create:dev', 'build:renderer:html', 'build:main:watch', 'build:renderer:watch'
));

gulp.task('cucumber', shell.task('npm run cucumber --'));

gulp.task('cucumber:watch', shell.task('nodemon --exec npm run cucumber:watch'));

gulp.task('test', gulp.series('build', 'cucumber'));

gulp.task('test:watch', gulp.series('build:watch', 'cucumber:watch'));

gulp.task('purge:translations', shell.task('rimraf ./translations/messages/source'));

gulp.task('electron:inspector', shell.task('npm run electron:inspector'));

gulp.task('start', shell.task(`cross-env NODE_ENV=${process.env.NODE_ENV || 'production'} electron ./`));

gulp.task('dev', gulp.series('server:create:dev', 'build:watch', 'server:start'));

gulp.task('debug', gulp.series('server:create:debug', 'build:watch', 'server:start', 'electron:inspector'));
