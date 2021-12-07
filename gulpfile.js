const gulp = require('gulp');
const shell = require('gulp-shell');
const flowRemoveTypes = require('gulp-flow-remove-types');

gulp.task('prepare:themes:utils', () =>
  gulp
    .src([
      'source/renderer/app/themes/utils/checkCreateTheme.js',
      'source/renderer/app/themes/utils/constants.js',
      'source/renderer/app/themes/utils/createShades.js',
      'source/renderer/app/themes/utils/createTheme.js',
      'source/renderer/app/themes/utils/findUpdates.js',
      'source/renderer/app/themes/utils/updateThemes.js',
      'source/renderer/app/themes/utils/updateThemesCLI.js',
      'source/renderer/app/themes/utils/writeThemeUpdate.js',
    ])
    .pipe(flowRemoveTypes())
    .pipe(gulp.dest('dist/utils'))
);

gulp.task('prepare:themes:daedalus', () =>
  gulp
    .src([
      'source/renderer/app/themes/daedalus/cardano.js',
      'source/renderer/app/themes/daedalus/dark-blue.js',
      'source/renderer/app/themes/daedalus/dark-cardano.js',
      'source/renderer/app/themes/daedalus/flight-candidate.js',
      'source/renderer/app/themes/daedalus/incentivized-testnet.js',
      'source/renderer/app/themes/daedalus/index.js',
      'source/renderer/app/themes/daedalus/light-blue.js',
      'source/renderer/app/themes/daedalus/shelley-testnet.js',
      'source/renderer/app/themes/daedalus/white.js',
      'source/renderer/app/themes/daedalus/yellow.js',
    ])
    .pipe(flowRemoveTypes())
    .pipe(gulp.dest('dist/daedalus'))
);

gulp.task('prepare:themes:scripts', () =>
  gulp
    .src([
      'source/renderer/app/themes/scripts/check.js',
      'source/renderer/app/themes/scripts/update.js',
    ])
    .pipe(flowRemoveTypes())
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

gulp.task('build:themes', gulp.series('clean:dist', 'prepare:themes'));

gulp.task(
  'test:e2e:nodemon',
  shell.task(
    'nodemon --watch dist --watch tests --exec "yarn test:e2e --tags \'@e2e and @watch\'"'
  )
);

gulp.task('test:e2e:watch', gulp.series('build:watch', 'test:e2e:nodemon'));

gulp.task('purge:translations', shell.task('rimraf ./translations/messages'));
