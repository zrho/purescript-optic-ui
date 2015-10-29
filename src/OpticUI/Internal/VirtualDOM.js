/* global exports, require */
"use strict";

// module OpticUI.Internal.VirtualDOM

// VTree -> HTMLElement
exports.createElement = function () {
  var vcreateElement = require('virtual-dom/create-element');
  return function (vtree) {
    return vcreateElement(vtree);
  };
}();

// VTree -> VTree -> Patch
exports.diff = function () {
  var vdiff = require('virtual-dom/diff');
  return function (a) {
    return function (b) {
      return vdiff(a, b);
    };
  };
}();

// forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement
exports.patch = function () {
  var vpatch = require('virtual-dom/patch');
  return function (p) {
    return function (node) {
      return function () {
        return vpatch(node, p);
      };
    };
  };
}();

// String -> VTree
exports.vtext = function () {
  var VText = require('virtual-dom/vnode/vtext');
  return function (s) { return new VText(s); };
}();

// Nullable String -> String -> Nullable String -> Props -> Array VTree -> VTree
exports.vnode = function () {
  var VirtualNode = require("virtual-dom/vnode/vnode");
  var SoftSetHook = require("virtual-dom/virtual-hyperscript/hooks/soft-set-hook");
  return function (ns) {
    return function (name) {
      return function (key) {
        return function (props) {
          return function (children) {
            if (name === "input" && props.value !== undefined) {
              props.value = new SoftSetHook(props.value);
            }
            return new VirtualNode(name, props, children, key, ns);
          };
        };
      };
    };
  };
}();

// forall a. Fn2 String a Props
exports.prop = function (key, value) {
  var props = {};
  props[key] = value;
  return props;
};

// Fn2 String String Props
exports.attrProp = function (key, value) {
  var props = { attributes : {} };
  props.attributes[key] = value;
  return props;
};

// forall eff e. Fn2 String (e -> Eff eff Unit) Props
exports.handlerProp = function (key, f) {
  var Hook  = function () {};
  var props = {};
  Hook.prototype.callback = function (e) {
    f(e)();
  };
  Hook.prototype.hook = function (node) {
    node.addEventListener(key, this.callback);
  };
  Hook.prototype.unhook = function (node) {
    node.removeEventListener(key, this.callback);
  };
  props["opticui-hook-" + key] = new Hook(f);
  return props;
};

// Fn2 Props Props Props
exports.concatProps = function () {
  var hOP = Object.prototype.hasOwnProperty;
  var copy = function (source, result) {
    for (var key in source) {
      if (hOP.call(source, key)) {
        if (key === "attributes") {
          var sourceAttrs = source[key];
          var resultAttrs = result[key];

          for (var attr in sourceAttrs) {
            if (hOP.call(sourceAttrs, attr)) {
              resultAttrs[attr] = sourceAttrs[attr];
            }
          }
        } else {
          result[key] = source[key];
        }
      }
    }
    return result;
  };
  return function (p, q) {
    return copy(p, copy(q, { attributes: {} } ));
  };
}();

// Props
exports.emptyProps = {};

//Fn2 String (HTMLElement -> Eff eff Unit) Props
exports.initializer = function(s, f){
  var Hook = function () {};
  Hook.prototype.hook = function (node) {
    if(!node.initialized){
      node.initialized = true;
      f(node)();
    }
  };
  var obj = {};
  obj[s] = new Hook(f);
  return obj;
};

//Fn2 String (HTMLElement -> Eff eff Unit) Props
exports.finalizer = function(s, f){
  var Hook = function () {};
  Hook.prototype.unhook = function (node) {
    f(node)();
  };
  var obj = {};
  obj[s] = new Hook(f);
  return obj;
};
