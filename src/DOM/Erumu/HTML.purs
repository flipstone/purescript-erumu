module DOM.Erumu.HTML
  ( module Attributes
  , module Elements
  , module Events
  ) where

import DOM.Erumu.HTML.Attributes (action, alt, ariaControls, ariaOrientation, ariaExpanded, ariaHaspopup, ariaHidden, ariaLabelledby, ariaModal, autocomplete, autofocus, cRadius, checked, classN_, class_, classes, clipRule, colSpan, cxCoord, cyCoord, data_, defaultValue, defer, dims, disabled, enctype, fill, fillRule, fontFamily, fontSize, for, gradientUnits, height, href, id_, method, name, noop, offset, placeholder, points, role, rows, rx, selected, spellcheck, src, stroke, strokeLinecap, strokeLinejoin, strokeWidth, style, tabindex, target, title, transform, type_, value, viewBox, width, x1Coord, x2Coord, xCoord, xmlns, y1Coord, y2Coord, yCoord) as Attributes
import DOM.Erumu.HTML.Elements (ElementFn, a, address, aside, br, button, circle, code, dd, div_, dl, dt, em, embed, fieldset, footer, form, graphic, h1, h2, h3,h4, h5, header, hr, i, iframe, img, input, label, li, line, linearGradient, main, nav, noscript, object, ol, option, p, path, polygon, rect, script, section, select, span, stop, strong, svg, table, tbody, td, text, textArea, th, thead, time, tr, ul) as Elements
import DOM.Erumu.HTML.Events (clickawayfn, onblur, onclick, onfocus, oninput, onmouseenter, onmouseleave, terminalOnclick) as Events
