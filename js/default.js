
/*
  This  file solely exists to import the CSS from katex. If we do this with
  an `@import` in postcss, webpack doesn't get informed of the original path,
  and only sees the path of `default.css`, hence can't find any of the KaTeX
  fonts.
*/

import { _ } from 'katex/dist/katex.css';

export default function nothing(_) { }