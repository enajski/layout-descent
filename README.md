# layout-descent

Measured search for the data layout of nested-map access patterns in Clojure code.

You state a task in a plan file: the levels of a nested map, the type of its leaves, the access
patterns the code runs, and how often. layout-descent compiles the plan to a Clojure namespace
with one specialized function for each pattern, proves that namespace against a reference model,
times it, and tells you whether another plan is better. A program or a language model drives it
through a command line that answers in JSON, or through the Vis extension in
`.vis/extensions/`.

It is a search, not gradient descent: the moves are discrete (add an order, change a format,
move a pattern), and a move is kept only when a measurement says so.

## Requirements

- Clojure CLI 1.12 and a JDK (developed on Java 25).
- Python 3.10 or later, only for the Vis extension and its test.

## Quick start

```sh
cd layout-descent
clojure -M:cli help
clojure -M:cli facts ../some-project/src       # what the code does to nested maps
cp examples/risk/plan.edn /tmp/plan.edn
clojure -M:cli verify /tmp/plan.edn            # gate: compile and compare with the model
clojure -M:cli descend /tmp/plan.edn           # greedy search, writes a candidate
clojure -M:cli accept /tmp/candidates/<task-hash>/<layout-hash>.edn --into /tmp/plan.edn
clojure -M:cli status /tmp/plan.edn
```

One run of the example on the development machine, weighted mean ns for each call:

| Plan | Orders | Cost | Step |
| --- | --- | --- | --- |
| `e471a80f71f4` | `[0 1 2] :hash` | 6773 | start |
| `85b647ff5a1b` | `[0 1 2] :hash`, `[2 0 1] :csf` | 217 | `descend`, -96.8 % |
| `4bd9518c54ef` | `[0 1 2] :csf`, `[2 0 1] :csf` | 185 | `descend`, -13.5 % |
| hand-written | `[0 1 2] :csf`, `[2 0 1] :csf`, `[0 1 2] :hash` | 174 | `evaluate`, -6.1 %, 3.4 times the memory |

The last row needs two decisions at once, so no neighbor move reaches it. A candidate may be any
valid plan of the task; `neighbors` and `descend` are only the mechanical baseline.

## The plan file

A plan is one EDN map. `examples/risk/plan.edn` is a complete one.

| Key | Meaning |
| --- | --- |
| `:layout/version` | `1` |
| `:name` | A lower-case symbol |
| `:levels` | One to four `{:id :type}`; the type is `:keyword`, `:long` or `:string` |
| `:leaf` | `{:type t}`; `t` is `:double`, `:long` or `:object` |
| `:patterns` | `{:id :bind [...] :fold f}`; `:bind` has `:key` or `:scan` for each level; `:fold` is `:sum`, `:count` or absent |
| `:orders` | `{:perm [...] :format f}`; `:perm` is the order in which the levels are stored; `f` is `:hash` or `:csf` |
| `:serve` | Pattern id to order index. Optional: the default is the order with the longest bound prefix |
| `:workload` | `:fanout` and `:fill` for each level, `:seed`, and `:weights` for each pattern |
| `:objective` | `:min-gain` (default `0.05`) and `:ns-per-byte` (default `0.0`), the price of memory |
| `:emit` | `{:ns some.ns :path "src/some/ns.clj"}`; the path is relative to the plan file |

The **task** is `:levels`, `:leaf`, `:patterns` and `:workload`. The **layout decisions** are
`:orders` and `:serve`. A candidate keeps the task and changes the decisions. The plan hash covers
the decisions, the task hash the task. A pattern may carry other keys, such as `:doc` or `:sites`;
they change no hash.

Formats:

- `:hash`: nested `java.util.HashMap`. The fastest point lookup; scans follow pointers.
- `:csf`: sorted arrays for each level, with the leaves in one primitive array. A count of a
  subtree is a pointer difference, a sum of a subtree is one loop over a contiguous span, and the
  memory is a fraction of `:hash`.

Each pattern becomes one function, named by its id:

| Pattern | Call | Result |
| --- | --- | --- |
| all `:key` | `(value ds tenant entity feature)` | The leaf, or `nil` |
| some `:scan`, no `:fold` | `(entity-features ds f init tenant entity)` | A reduction; `f` gets `acc`, the scanned keys, the leaf |
| `:fold :sum` | `(feature-total ds feature)` | The sum of the leaves |
| `:fold :count` | `(tenant-size ds tenant)` | The number of leaves |

`(build nested-map)` in the same namespace makes `ds`. `status` prints the exact call of every
pattern. Key types are checked at `build`: a `:long` level takes integers in the signed 64-bit range.
An Integer key finds a Long key; `nil`, fractional and out-of-range query keys miss.
Scan order is unspecified. Use a reducer whose result does not depend on visitation order.
Returning `(reduced value)` stops the scan and returns `value`.

## Commands

Every command prints one JSON document on stdout, with snake_case keys and `"ok"`. Nothing else
goes to stdout.

| Command | Writes | Purpose |
| --- | --- | --- |
| `facts <src-dir>... [--limit N]` | nothing | Path shapes of `get-in`, `assoc-in`, `update-in`, `get`, `reduce-kv` and others, with sites |
| `check <plan>` | nothing | Validate; hash, task, warnings or problems |
| `status <plan>` | nothing | Orders, patterns, calls, emitted file state, last journal records |
| `verify <plan> [--source <trusted-clojure-file>]` | nothing for generated code | The gate; `--source` executes the supplied file |
| `neighbors <plan> [--out-dir <dir>]` | candidate files | Every plan one decision away |
| `evaluate <candidate> --against <plan>` | journal | Gate both, time both, verdict |
| `descend <plan> [--max-steps N] [--out <file>]` | one candidate, journal | Greedy search |
| `accept <candidate> --into <plan> [--force <reason>]` | plan, emitted file, journal | Take the candidate's decisions |
| `emit <plan> [--out <file>]` | optional preview file | Gate and preview the namespace; only `accept` writes the configured output |

Exit code 0: done. Exit code 1: refused or failed; `error.code` names the cause and `error.hint`
the next step. Exit code 2: wrong usage or an internal error.

## What keeps the search honest

- **The gate.** `layout-descent.model` answers every pattern from the plain nested map. Emitted
  code passes only when it compiles with no reflection or boxed-math warning and agrees with the
  model on five datasets (empty, one entry, empty inner maps, sparse, dense), with keys that hit,
  keys that miss and Integer keys. `evaluate`, `emit` and `accept` run it first.
- **One task.** Costs compare only between plans with the same task hash. `accept` refuses a
  candidate of another task, with or without `--force`.
- **Same data, interleaved rounds.** All plans of a measurement are built from one seeded
  dataset, called with the same arguments, and timed in turn. The cost is the weighted median
  ns for each call, plus `:ns-per-byte` times the estimated bytes. Both costs use the baseline’s
  objective. Searches time candidates in bounded groups, each against its own baseline run.
- **A verdict needs a clear difference.** `better` and `worse` need at least `:min-gain` and more
  than three times the summed spread of the samples. Everything else is `no-change`.
- **Holdout.** A `better` verdict is measured again on data of another seed, and stays `better`
  only when that agrees. `descend` measures its chosen neighbor again on its own, which removes
  the luck of picking the minimum of several noisy numbers.
- **One writer.** Only `accept` writes an accepted plan and its emitted file. It needs a `better`
  verdict from the latest evaluation of those two hashes under the current objective in
  `<plan>.journal`, or `--force` with a reason, which the journal keeps. Older journal entries
  without an objective must be remeasured. It moves only `:orders` and `:serve`. Running it again
  changes nothing unless the emitted file is missing or stale. If publication is interrupted, fix
  the reported write problem and rerun `accept`; it finishes the recorded change before starting
  another. Do not remove the per-plan lock file while a command is running.

## Facts

`facts` uses clj-kondo to find the calls that resolve to the `clojure.core` map functions and
edamame to read their arguments. In a shape, `?` is a key that the code computes, `*` a scan and
anything else a constant:

```text
[":scores" "?" "?" ":risk"]   2 reads, 2 writes   -> two :key levels under a constant
[":scores" "?" "*"]           1 scan              -> a pattern [:key :scan]
```

Nested accesses are joined: `(get (get-in db [:scores t]) e)` is one site at `[:scores ?t ?e]`.
A function handed to `swap!` is a write to the reference. A path that is not a literal vector is
counted as `dynamic`. Facts decide nothing: you, or the model, write the plan from them.

## Vis extension

`.vis/extensions/layout_descent_tools.py` registers the namespace `descent` with one tool for each
command: `descent.facts`, `check`, `status`, `verify`, `neighbors`, `evaluate`, `descend`,
`accept` and `emit`. Each tool runs the command line and returns its answer as a dict. A refusal
is a returned answer with `ok` false; only a usage or internal error raises. Vis loads the file
when this project is in the workspace; reload Vis after a change.

## Use an emitted layout

Add layout-descent to the classpath of the project, because the emitted namespace needs
`layout-descent.rt`. That brings in Clojure only: clj-kondo, edamame and data.json belong to the
`:cli` alias of the tool.

```clojure
;; deps.edn of the project
{:deps {layout-descent/layout-descent {:local/root "../layout-descent"}}}
```

```clojure
(require '[demo.risk-layout :as risk])
(def ds (risk/build nested-map))
(risk/value ds :acme 42 :score)
(risk/feature-total ds :score)
```

Commit the plan and the emitted file together. `status` says `stale` when they differ.

## Tests

```sh
clojure -M:cli:test              # plan, gate, bench, facts, command line
python3 test/extension_test.py   # the Vis entry file, with a stub of the SDK
```

## Limits

- A dataset is built once from a nested map and is read-only. Writes that `facts` reports stay
  on the map; rebuild the dataset after them.
- The workload is synthetic: fanout, fill, seed and weights. It is your statement of the task,
  not a replay of production traffic.
- Call sites are not rewritten for you. `facts` gives the sites and `status` the calls.
- `:bytes` is an estimate from entry counts, not a heap measurement.
- At most four levels. One leaf type for each plan.
- `neighbors` proposes an order only when it gives some pattern a longer bound prefix.
