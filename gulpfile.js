const gulp = require('gulp');
const shell = require('gulp-shell');

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

gulp.task('clean:dist', shell.task('rimraf ./dist'));

gulp.task('build:themes', gulp.series('clean:dist', 'prepare:themes'));

gulp.task(
  'test:e2e:nodemon',
  shell.task(
    'nodemon --watch dist --watch tests --exec "yarn test:e2e --tags \'@e2e and @watch\'"'
  )
);

gulp.task('e2e:watch', gulp.series('clean:dist', shell.task('yarn dev')));

gulp.task('test:e2e:watch', gulp.series('e2e:watch', 'test:e2e:nodemon'));
