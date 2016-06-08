/* global exports, require */
"use strict";

// module OpticUI.Util.Lazy

exports.suspend = function(run) { return function() { return run({})(); } };
