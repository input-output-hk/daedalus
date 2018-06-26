const gulp = require('gulp');
const webpack = require('webpack');
const webpackStream = require('webpack-stream');
const mainWebpackConfig = require('./source/main/webpack.config');
const rendererWebpackConfig = require('./source/renderer/webpack.config');
const shell = require('gulp-shell');
const electronConnect = require('electron-connect');

const mainOutputDestination = () => gulp.dest('dist/main');
const rendererOutputDestination = () => gulp.dest('dist/renderer');

const buildMain = (config, done) => gulp.src('source/main/index.js')
  .pipe(webpackStream(Object.assign({}, mainWebpackConfig, config), webpack, done))
  .pipe(mainOutputDestination());

const buildRenderer = (config, done) => gulp.src('source/renderer/index.js')
  .pipe(webpackStream(Object.assign({}, rendererWebpackConfig, config), webpack, done))
  .pipe(rendererOutputDestination());

// Setup electron-connect server to start the app in development mode
let electronServer;

const createElectronServer = (env, args = []) => {
  electronServer = electronConnect.server.create({
    spawnOpt: {
      env: Object.assign({}, process.env, env),
      args,
    }
  });
};

const startElectronInWatchMode = () => {
  electronServer.start();
  gulp.watch('dist/main/index.js', gulp.series('electron:restart'));
  gulp.watch('dist/renderer/*', gulp.series('electron:reload'));
};

gulp.task('clear-cache', shell.task('rimraf ./node_modules/.cache'));

gulp.task('clean:dist', shell.task('rimraf ./dist'));

gulp.task('build:main', (done) => buildMain({}, done));

gulp.task('build:main:watch', (done) => buildMain({ watch: true }, done));

gulp.task('build:renderer:html', () => gulp.src('source/renderer/index.html').pipe(gulp.dest('dist/renderer/')));

gulp.task('build:renderer:assets', (done) => buildRenderer({}, done));

gulp.task('build:renderer', gulp.series('build:renderer:html', 'build:renderer:assets'));

gulp.task('build:renderer:watch', (done) => buildRenderer({ watch: true }, done));

gulp.task('build', gulp.series('clean:dist', 'build:main', 'build:renderer'));

gulp.task('build:watch', gulp.series('clean:dist', 'build:renderer:html', 'build:main:watch', 'build:renderer:watch'));

gulp.task('cucumber', shell.task('npm run cucumber --'));

gulp.task('cucumber:watch', shell.task('nodemon --exec npm run cucumber:watch'));

gulp.task('test', gulp.series('build', 'cucumber'));

gulp.task('test:watch', gulp.series('build:watch', 'cucumber:watch'));

gulp.task('purge:translations', shell.task('rimraf ./translations/messages/source'));

gulp.task('electron:inspector', shell.task('npm run electron:inspector'));

gulp.task('electron:restart', (done) => {
  electronServer.restart();
  done();
});

gulp.task('electron:reload', (done) => {
  electronServer.reload();
  done();
});

gulp.task('start:watch', () => {
  createElectronServer({ NODE_ENV: process.env.NODE_ENV || 'development' });
  startElectronInWatchMode();
});

gulp.task('start:debug', () => {
  createElectronServer({ NODE_ENV: 'development' }, ['--inspect', '--inspect-brk']);
  startElectronInWatchMode();
});

gulp.task('start', shell.task(`cross-env NODE_ENV=${process.env.NODE_ENV || 'production'} electron ./`));

gulp.task('dev', gulp.series('build:watch', 'start:watch'));

gulp.task('debug', gulp.series('build:watch', 'start:debug', 'electron:inspector'));
