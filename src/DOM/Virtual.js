import * as H from 'virtual-dom/h.js';
import * as Diff from 'virtual-dom/diff.js';
import * as Patch from 'virtual-dom/patch.js';
import * as CE from 'virtual-dom/create-element.js';

'use strict';

// These are the namespaced element attributes we want to render that
// virtual-dom.js will not
const NAMESPACED_ATTRS = [ "circle"
                         , "class"
                         , "clip-rule"
                         , "d"
                         , "data-clipboard-text"
                         , "data-leg-id"
                         , "data-popup-name"
                         , "data-popup-opens"
                         , "fill-rule"
                         , "fill"
                         , "gradientUnits"
                         , "height"
                         , "line"
                         , "offset"
                         , "points"
                         , "r"
                         , "rect"
                         , "rx"
                         , "stroke-linecap"
                         , "stroke-linejoin"
                         , "stroke-width"
                         , "stroke"
                         , "tabindex"
                         , "transform"
                         , "viewBox"
                         , "width"
                         , "x"
                         , "x1", "x2", "y1", "y2", "cx", "cy"
                         , "y"
                         ];

export function unsafeValue(v) { return v; }

export function node(name) {
  return function (attributeList) {
    return function (children) {
      var attrs = {};
      var nsAttrs = {};

      for (var i = 0; i < attributeList.length; i++) {
        var a = attributeList[i];
        if (NAMESPACED_ATTRS.some(v => v == a.key)) {
          nsAttrs[a.key] = a.value;
        } else {
          attrs[a.key] = a.value;
        }
      }

      // How we get bespoke/namespaced attributes past virtual-dom syntax checks
      attrs.attributes = nsAttrs;

      return H.default(name, attrs, children);
    }
  }
}

export function nonPropagatingEventHandler(eff) {
  return function(event) {
    eff(event)();
      if(typeof event.stopPropagation == "function") {
        event.stopPropagation();
      } else {
        event.cancelBubble = true;
      }
  }
}

export function propagatingEventHandler(eff) {
  return function(event) {
    eff(event)();
  }
}

export function createElement(tree) {
  return function() {
    return CE.default(tree);
  }
}

export function diff(oldTree) {
  return function (newTree) {
    return Diff.default(oldTree, newTree);
  }
}

export function patchElement(patches) {
  return function (node) {
    return function() {
      return Patch.default(node, patches);
    }
  }
}

export function text(s) { return s };

export function hookFn(hook) {
  return function(node) {
    hook(node)();
  }
}

