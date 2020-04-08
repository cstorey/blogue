import { default as AutoKaTeX } from "katex/contrib/auto-render/auto-render";

import { _ } from "katex/dist/katex.css";

export default function renderMaths(document) {
  document.querySelectorAll(".math").forEach(AutoKaTeX);
}
