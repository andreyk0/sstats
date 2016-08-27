# sstats


````
Reads a stream of numbers from STDIN, computes a few stats.

Usage: sstats [-a|--average] [-c|--count] [-M|--max] [-m|--min] [-s|--sum]
              [-v|--var] [-F|--fp-format ARG] [-D|--num-dec-places ARG]
  Reads a stream of numbers from STDIN, computes a few stats.Singular stats are
  printed directly, multiple stats are prefixed with the name.If no options are
  given - all stats are printed by default.AVG and VAR are computed with Double
  precision, counter is 64b and SUM/MIN/MAX use Scientific type.

Available options:
  -h,--help                Show this help text
  -a,--average             Print average.
  -c,--count               Print record count.
  -M,--max                 Print max.
  -m,--min                 Print min.
  -s,--sum                 Print sum.
  -v,--var                 Print variance.
  -F,--fp-format ARG       Floating point format, 'e' (exponent), 'f' (fixed) or
                           'g' (generic). (default: Fixed)
  -D,--num-dec-places ARG  Number of decimal places in the output. (default: 3)
````

```bash
# sstats
1
2
3
<Ctrl-D>
AVG 2.000
CNT 3.000
MAX 3.000
MIN 1.000
SUM 6.000
VAR 1.000
```
