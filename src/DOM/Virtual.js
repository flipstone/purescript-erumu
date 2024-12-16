import * as H from 'virtual-dom/h.js';
import * as Diff from 'virtual-dom/diff.js';
import * as Patch from 'virtual-dom/patch.js';
import * as CE from 'virtual-dom/create-element.js';

'use strict';

// These are the namespaced element attributes we want to render that
// virtual-dom.js will not. This includes special keyword attributes like "class" and "for" and also svg attributes
//
// Since our `Prop` type combines the notion of things like attributes and event handlers
// if we tried in `node` to add all attributes instead of this explicit whitelist we would incorrectly get html like
// "className=" instead of "class=" and things like "onclick=function(a){(e)}..." when the onclick event
// shouldn't be on the html at all
const NAMESPACED_ATTRS = [ "circle"
                         , "class"
                         , "clip-rule"
                         , "d"
                         , "fill-rule"
                         , "fill"
                         , "for"
                         , "gradientUnits"
                         , "height"
                         , "line"
                         , "offset"
                         , "points"
                         , "preserveAspectRatio"
                         , "spellcheck"
                         , "r"
                         , "rect"
                         , "required"
                         , "rx"
                         , "tabindex"
                         , "transform"
                         , "viewBox"
                         , "width"
                         , "x"
                         , "x1", "x2", "y1", "y2", "cx", "cy"
                         , "y"
                         ];

// We allow all attributes that start with these strings instead of explicitly listing them in `NAMESPACED_ATTRS`
const NAMESPACED_ATTRS_PREFIXES = [ "aria"
                                  , "data"
                                  , "stroke"
                                  ];

export function unsafeValue(v) { return v; }

export function node(name) {
  return function (attributeList) {
    return function (children) {
      var attrs = {};
      var nsAttrs = {};

      for (var i = 0; i < attributeList.length; i++) {
        var a = attributeList[i];
        if (NAMESPACED_ATTRS.some(v => v == a.key) || NAMESPACED_ATTRS_PREFIXES.some(prefix => a.key?.startsWith(prefix))) {
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

// Handles only 0 clicks (keyboard)
// and single mouse clicks
export function propagatingNonMultiplePointerEventHandler(eff) {
  return function(event) {
    if (!(event instanceof PointerEvent) || event.detail > 1) {
      return;
    }
    eff(event)();
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

export function preventDefaultEventHandler(eff) {
  return function(event) {
    eff(event)();
      if(typeof event.preventDefault == "function") {
        event.preventDefault();
      }
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
