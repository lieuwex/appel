# appel

An arbitrary precision rational calculator inspired by APL. Numbers are exact rationals
(`rug::Rational`, GMP); floats only appear for things like `sin`, `log` and fractional powers.

## Commands

- `cargo test` — all tests are unit tests inside the source files (`tests/main.rs` is empty).
- `cargo build --release && ./target/release/appel '<expr>'` — evaluate one expression. Without
  an expression it reads lines from stdin, keeping variables between lines.
- Use `--` for expressions that start with `-`: `appel -- '-5km'`, otherwise clap sees a flag.
- `-f float` / `-f 3` / `-f ratio` sets the output format; `--pre file.appel` runs a prelude.
- Try things with the **release** binary: debug builds print `parsed as: ...` and a backtrace at
  every `collect_point!()`.

### Benchmarks

`benches/bench.sh [-n RUNS] [-b BINARY] [FILTER]` runs end-to-end cases and prints min/median.
To compare with an older version, build that version and pass it with `-b`.

- This machine has a hybrid CPU (cores 0–7 are performance cores). Unpinned runs vary by ±15%
  or more depending on which core they land on. For a reliable comparison, pin and count
  instructions: `taskset -c 2 perf stat -r 30 -e instructions ./target/release/appel '<expr>'`.
- `benches/benchmark.rs` is an unfinished criterion stub; criterion isn't a dependency, so
  `cargo bench` doesn't work.

## Layout

- `src/parser/lexer.rs` — tokens. `src/parser/mod.rs` — recursive descent parser; the module doc
  has the full precedence table.
- `src/ast.rs` — `Expr`, `Statement`, operators. `Display` on `Expr` is used by parser tests.
- `src/executor/executor.rs` — evaluation. `call_binary` and `execute_unary` hold every operator.
- `src/executor/chain.rs` — `Chain` (a value or a lazy `IterShape` iterator) and output formatting.
- `src/executor/matrix.rs` — `Matrix` (a collected vector) and `Formatter`.
- `src/units.rs` — units: dimensions, the unit table, lexing unit expressions.
- `src/executor/num_type.rs`, `values.rs`, `src/grammar.pest` — untracked leftovers, not compiled.

## Language notes

- Juxtaposition is a vector (`1 2 3`) or a function call (`f x y`). Whitespace matters: `1 -2` is
  a vector, `1 - 2` and `1-2` are subtractions, `a [1]` is not an index.
- Vectors are flat: no nesting. Lambdas capture their environment by copying all variables
  (this `HashMap` clone dominates lambda-heavy benchmarks).
- Earlier results, like `%` in Mathematica: `_` is the last result, `__` the one before, `_5`
  the fifth (from 1). Every line that gives a value counts, assignments too; errors and `)n`
  don't. These are `TokenKind::Out` / `Atom::Out`, not names, so they can't be assigned.
  Lambdas and `let` copy the results they use into their environment when created; named
  functions look them up when called. Other names starting with `_` (`_a`, `_5a`) are ordinary
  variables. The interactive prompt shows the number the next result gets: `[5]:`.
- Evaluation is lazy where possible (`Chain::Iterator`); `collect_point!()` marks where a vector
  gets collected.

## Parser stack depth

`parser::tests::test_paren` parses 200 nested parentheses. Each level goes through about 12
parser functions holding `Expr` values, and the debug test thread has a 2 MiB stack, so this
test sits close to a stack overflow. Making `Expr` (or anything it holds, like `Unit`) larger
can make it overflow. This is why `Dim` uses `i8` exponents.

## Units

Syntax:

- A unit must directly follow a number or `)`: `5km`, `(1 2 3)km`. `5 km` is a vector.
- Unit expressions: names joined by `.` (multiply) and `/` (divide), with `^n` powers:
  `kg.m^2/s^2`, `s^-1`. `/` only divides by the unit right after it: `m/s/A` is m·s⁻¹·A⁻¹.
  `^` is used instead of `**` so the exponent belongs to the unit.
- `x to km/h` converts. `to` is a keyword (so not a valid variable name) and binds loosest:
  `x + 1m to km` converts the whole sum; inside `let a = ... in` the value needs parentheses.
- After a `.` or `/`, the unit only continues if the next word is a known unit: `5m/s` is
  metres per second even if a variable `s` exists, while `5m/x` is `5m` divided by `x`.
- A unit binds tighter than any operator: `1/2s` is `1 / (2s)`, `-3/2m/s` is `-3 / (2m/s)`.
- An unknown unit after a number is a lex error (`5x`); after `)` it falls back to an identifier.

Representation:

- Values are always stored in SI base units. A `Unit` is a `Dim` (exponents of m, kg, s, A, K,
  mol, cd) plus an optional display unit. The unit belongs to the whole vector, not to each
  element, so element iterators don't change.
- Display units are lists of named units with exponents (`NamedUnit`), not text, so `*`, `/` and
  `**` combine them: `(2km) ** 2` is `4km^2`, `5km/h * 2h` is `10km` (same names merge/cancel),
  `1m.m` is shown as `1m^2`.
- Invariant: dimensionless values never have a display unit (`1km / 1m` is `1000`).
- The unit table is in `UNITS` in `src/units.rs`; prefixable units take `PREFIXES`. Hecto and
  deca are left out because `h` and `d` are hours and days. The gram is the prefixable mass unit
  (`kg` is `k` + `g`). No °C/°F (affine) and no angle units.

Operator rules (in `call_binary` / `execute_unary`):

- `+ - % max min , union` need equal dimensions; the result keeps the left side's display unit,
  or the right's if the left has none. Comparisons and `in` need equal dimensions and return
  plain numbers. Mixing a unit with a plain number is an error (`5m + 1`).
- `*`, `/` combine dimensions. With a plain number on one side the display unit is kept.
- `**` needs a single plain-number exponent if the base has a unit. Fractional powers work when
  the dimension stays integer (`(4m^2) ** (1/2)` is `2m`); the display unit is kept only if its
  exponents stay integer too, otherwise SI. Exponents are `i8`: `(2m) ** 200` errors; plain
  numbers have no such limit.
- `sin cos tan log ! ?i iota << >> pack unpack` and indices need plain numbers. `drop rho mask pad`
  need a plain left side and keep the right side's unit. The `pad` value is in the vector's
  display unit.
- `floor ceil round` work in the display unit: `floor 1.5km` is `1km`.
- `rho`, `up`, `down`, `sgn` return plain numbers.
- Vector literals, `,`, maps (`f . xs`) and scans (`f \\ xs`) require every element to end up
  with the same dimension: `*\\(1 2)m` is an error. Folds don't, so `*//(1 2 3)m` is `6m^3`.
- A computed display unit that only contains SI base units is dropped, so in
  `(2m * 3s / 1s) + 1km` the result is shown in km. A typed unit always keeps its name: `5m + 1km`
  is in m, `1m.s` stays `m.s`.
- When one side has no display unit it is treated as SI base units when combining: `1km * x`
  with `x` in m² gives `km.m^2`.

Output:

- Output can be parsed again: `5km`, `(3/2)km`, `(1 2 3)km`. Parentheses are added for vectors
  and whenever the number contains `/`. The `)n` command uses the same rule.
- Formatting only divides by the display factor when there is a display unit; plain numbers are
  formatted directly (dividing every value by 1 made printing ~75% slower).
- Units with only negative exponents print as `s^-1`, since there is no `1/s` syntax.
