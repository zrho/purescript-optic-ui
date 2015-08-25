'use strict'

var gulp        = require('gulp');
var purescript  = require('gulp-purescript');
var browserify  = require('browserify');
var source      = require('vinyl-source-stream');
var rimraf      = require("rimraf");

process.env['NODE_PATH'] = __dirname + '/purescript_modules';

var sources =
  [ 'src/**/*.purs'
  , 'bower_components/purescript-*/src/**/*.purs'
  ];

var foreigns =
  [ 'src/**/*.js'
  , 'bower_components/purescript-*/src/**/*.js'
  ];

gulp.task('make', function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("clean-output", function (cb) {
  rimraf("output", cb);
});

gulp.task("clean", ["clean-output"]);

gulp.task('browserify', ['make'], function () {
  var b = browserify({
    entries: './app.js',
    debug: true,
  });

  return b.bundle()
    .pipe(source('app.js'))
    .pipe(gulp.dest('./output/'));
});

gulp.task('default', ['browserify']);
