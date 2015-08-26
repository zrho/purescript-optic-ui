'use strict'

var gulp        = require('gulp');
var purescript  = require('gulp-purescript');
var browserify  = require('browserify');
var source      = require('vinyl-source-stream');
var rimraf      = require("rimraf");

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

gulp.task("clean-docs", function (cb) {
  rimraf("docs", cb);
});

gulp.task("clean", ["clean-docs", "clean-output"]);

gulp.task("docs", ["clean-docs"], function() {
  return purescript.pscDocs({
    src: sources,
    docgen: 
    {
      "OpticUI.Components": "docs/OpticUI/Components.md",
      "OpticUI.Core": "docs/OpticUI/Core.md",
      "OpticUI.HTML.Attributes": "docs/OpticUI/HTML/Attributes.md",
      "OpticUI.HTML.Elements": "docs/OpticUI/HTML/Elements.md",
      "OpticUI.HTML.Handlers": "docs/OpticUI/HTML/Handlers.md",
      "OpticUI.HTML": "docs/OpticUI/HTML.md"
    }
  });
});

gulp.task('default', ['make']);
