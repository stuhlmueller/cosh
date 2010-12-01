#!r6rs

(import (rnrs)
        (scheme-tools)
        (church desugar))

(pretty-print
 (de-sugar-all '%(expr)s))