#!/usr/bin/env node

var pandoc = require('pandoc-filter');
var KaTeX = require('katex');

function action(type,value,_,_) {
    if (type === 'Math') {
        let displayMode = value[0].t == 'DisplayMath';
        let rendered = KaTeX.renderToString(value[1], {
            throwOnError: true,
            displayMode: displayMode
        })
        return pandoc.RawInline('html', rendered);
    }
}

pandoc.stdio(action);