# layout-descent

Measured search for the data layout of nested-map access patterns. README.md has the plan
format, the commands and the rules of the search.

## Checks

- `clojure -M:cli:test` runs every Clojure test (about 30 s). `clojure -M:cli:test facts plan`
  runs only `facts_test.clj` and `plan_test.clj`; a new `test/layout_descent/<name>_test.clj` is
  found by itself.
- `clojure -M:cli <command>` runs the tool. The base classpath holds only what emitted code needs: keep tool dependencies in the `:cli` alias.
- `python3 test/extension_test.py` tests `.vis/extensions/layout_descent_tools.py`.
- Run both after a change in `src/` or `.vis/`.

## Layout of the code

| Namespace | Owns |
| --- | --- |
| `layout-descent.plan` | The plan format: validation, normal form, hashes, neighbors, rendering |
| `layout-descent.model` | Reference semantics on the plain nested map |
| `layout-descent.rt` | Dataset types and `build-dataset`; the only runtime that emitted code needs |
| `layout-descent.emit` | Plan to source text. Deterministic |
| `layout-descent.gen` | Seeded data, argument samples, edge datasets |
| `layout-descent.gate` | Load emitted source, capture warnings, compare with the model |
| `layout-descent.bench` | Measurement and the verdict |
| `layout-descent.facts` | clj-kondo and edamame analysis of call sites |
| `layout-descent.cli` | The JSON command surface, the journal, `accept` |

## Invariants

- The model decides what is correct. A change to `emit` or `rt` is done when the gate passes for
  every permutation, format and leaf type (`gate_test.clj`), not before.
- Emitted source compiles with no reflection or boxed-math warning. The gate fails on a warning.
- `plan/plan-hash` is written into journals. `plan_test.clj` pins the hash of the example: change
  it only when the layout format changes on purpose.
- `accept` is the only writer of an accepted plan and of its emitted file. `neighbors`, `descend`
  and `evaluate` write candidates and journal lines only. `measure`, `journal` and `status` write
  nothing.
- A new optional plan key is absent from the normal form when the plan does not give it, so the
  hashes of existing plans and their journals stay valid.
- stdout of the command line holds one JSON document. Keys are snake_case. A map keyed by pattern
  ids must not go to the output, because the key conversion would change the ids: use a list.
- A new command needs an entry in `cli/commands` and `flags-of`, a hint for each new error code,
  a method in the Vis entry file, and a row in README.md.
- A new format needs a builder in `rt`, an emitter in `emit`, an entry in `plan/formats`, and no
  change in `model`.
