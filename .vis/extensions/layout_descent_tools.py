"""Vis tools for layout-descent.

Each tool runs one command of `layout-descent.cli` in a subprocess and returns its JSON answer as
a dict. The command line is the contract; this file adds no rule of its own.
"""

from __future__ import annotations

import json
import subprocess
from pathlib import Path
from typing import Annotated, Any

import blockether.vis.extension as vis

ROOT = Path(__file__).resolve().parents[2]
TIMEOUT_S = 1800

PROMPT = f"""\
layout-descent ({ROOT}) searches for the data layout that serves the access patterns of a nested
map fastest. A plan file (EDN) states the task (:levels, :leaf, :patterns, :workload) and the
layout decisions (:orders, :serve). The tools are in the `descent` namespace.

The loop:
1. `descent.facts([src_dir])`: the path shapes of the code. `?` is a :key level, `*` a :scan level.
   `hidden` counts the shallow shapes and the write sites that the answer leaves out.
2. Write a plan with one order. `{ROOT}/examples/risk/plan.edn` is a model; `{ROOT}/README.md` has the keys.
   Put the grounding of the plan in :notes: `accept` keeps :notes and drops comments.
3. `descent.check(plan)`, then `descent.verify(plan)`.
4. `descent.descend(plan)` walks the neighbors. For other candidates use
   `descent.neighbors(plan, out_dir)`, or write one. A candidate keeps the task and changes only
   :orders and :serve.
5. `descent.evaluate(candidate, against=plan)`. The verdict is `better`, `worse` or `no-change`.
6. `descent.accept(candidate, into=plan)` only after a `better` verdict.

For a report: `descent.descend` gives the cost, the bytes and the ns of each pattern for every step
and writes each chosen plan. `descent.measure([plan, ...])` times plans in one run and writes no
journal record. `descent.journal(plan)` gives every record.

Rules:
- `accept` is the only writer of an accepted plan and of its emitted file. Do not edit either.
- Do not change :workload or :objective to make a candidate win. They are the user's statement of
  the task: ask the user.
- Call these tools, not the command line in a shell. Each call starts a JVM: seconds for most,
  under a minute for `descend` on 25,000 entries. The tool waits up to 30 minutes.
- A `no-change` verdict supports no claim of direction. Do not say which of the two plans is faster.
- Report the numbers of one run: one `descend`, `evaluate` or `measure` answer for each table.
- When `sensitivity.depends_on_weights` is true, other :weights can reverse the verdict. Say so,
  with the smallest `flip_factor`.
- Say what the plan models when that is not the code the user named. Compare the result with the
  strongest layout that exists already, not only with the starting plan.
- An answer with `ok` false has `error.code` and `error.hint`. Follow the hint.
- Relative paths resolve against {ROOT}. Give absolute paths for files of another project.
"""


class DescentError(RuntimeError):
    """The command line was used wrongly or failed internally (exit code 2)."""


def _path(path: str) -> str:
    """An absolute path; a relative one resolves against the layout-descent project."""
    p = Path(path).expanduser()
    return str(p if p.is_absolute() else ROOT / p)


def run_cli(*args: str) -> dict[str, Any]:
    """Run one command and return its answer. A refusal (exit 1) is an answer, not an exception."""
    proc = subprocess.run(
        ["clojure", "-M:cli", *args],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=TIMEOUT_S,
    )
    try:
        answer = json.loads(proc.stdout)
    except ValueError as exc:
        raise DescentError(
            f"layout-descent printed no JSON (exit {proc.returncode}): {proc.stderr.strip()[-800:]}"
        ) from exc
    if proc.returncode == 2:
        error = answer.get("error", {})
        raise DescentError(f"{error.get('code')}: {error.get('message')} {error.get('hint')}")
    return answer


def _flags(**flags: Any) -> list[str]:
    """Command-line flags for the values that are set."""
    out: list[str] = []
    for name, value in flags.items():
        if value is not None:
            out += ["--" + name.replace("_", "-"), str(value)]
    return out


# ---- activity ----------------------------------------------------------------


def _percent(gain: Any) -> str:
    return "n/a" if gain is None else f"{100 * gain:+.1f} %"


def _pattern_table(result: dict[str, Any]) -> tuple[Any, ...]:
    base = {p["pattern"]: p for p in result["baseline"]["patterns"]}
    rows = [
        [p["pattern"], str(p["weight"]), f"{base[p['pattern']]['ns']:.0f}", f"{p['ns']:.0f}"]
        for p in result["candidate"]["patterns"]
    ]
    return (vis.ActivityTable(["Pattern", "Weight", "Plan ns", "Candidate ns"], rows),)


def _summaries(command: str, result: dict[str, Any]) -> tuple[str, str, tuple[Any, ...]]:
    """Headline, one-line summary and content for the answer of one command."""
    if command == "facts":
        shapes = result["shapes"]
        rows = [[" ".join(s["shape"]), str(s["reads"]), str(s["writes"]), str(s["scans"])] for s in shapes[:12]]
        hidden = result.get("hidden") or {}
        return (
            "Access facts",
            f"{result['site_count']} sites in {result['files']} files, {len(shapes)} path shapes shown, "
            f"{hidden.get('shapes', 0)} hidden",
            (vis.ActivityTable(["Shape", "Reads", "Writes", "Scans"], rows),) if rows else (),
        )
    if command == "check":
        return ("Plan is valid", f"Plan {result['hash']}, {len(result['warnings'])} warnings", ())
    if command == "status":
        rows = [
            [p["pattern"], f"{p['perm']} {p['format']}", f"{p['indexed_levels']}/{p['bound_levels']}", p["call"]]
            for p in result["patterns"]
        ]
        return (
            f"Plan {result['name']} {result['hash']}",
            f"{len(result['orders'])} orders, emitted file {result['emitted']['state']}",
            (vis.ActivityTable(["Pattern", "Order", "Indexed", "Call"], rows),),
        )
    if command == "journal":
        return ("Journal", f"{len(result['entries'])} of {result['records']} records", ())
    if command == "measure":
        plans = result["plans"]
        ids = [p["pattern"] for p in plans[0]["patterns"]]
        rows = [
            [p["hash"], f"{p['cost']:.1f}", str(p["bytes"]), *[f"{row['ns']:.1f}" for row in p["patterns"]]]
            for p in plans
        ]
        return (
            "Plans measured",
            f"{len(plans)} plans of task {result['task']} on {result['entries']} entries",
            (vis.ActivityTable(["Hash", "Cost", "Bytes", *[f"{i} ns" for i in ids]], rows),),
        )
    if command == "verify":
        gate = result["gate"]
        return ("Gate passed", f"{gate['checks']} checks against the model, plan {gate['hash']}", ())
    if command == "neighbors":
        rows = [[n["hash"], n["move"]["op"], n["why"]] for n in result["neighbors"]]
        return (
            "Neighbor plans",
            f"{len(rows)} plans one decision from {result['hash']}",
            (vis.ActivityTable(["Hash", "Move", "Why"], rows),) if rows else (),
        )
    if command == "evaluate":
        return (
            f"Candidate is {result['verdict']}",
            f"Cost {_percent(-result['gain'])}, {result['candidate']['hash']} against {result['baseline']['hash']}",
            _pattern_table(result),
        )
    if command == "descend":
        taken = [s["chosen"] for s in result["steps"] if s.get("chosen")]
        rows = [[c["hash"], c["move"]["op"], _percent(c["gain"]), f"{c['cost']:.1f}", c["why"]] for c in taken]
        return (
            f"Descent is {result['verdict']}",
            f"{len(taken)} steps from {result['start']} to {result['final']}. {result['stopped']}",
            (vis.ActivityTable(["Hash", "Move", "Gain", "Cost", "Why"], rows),) if rows else (),
        )
    if command == "accept":
        if not result["changed"]:
            return ("Plan unchanged", f"Plan is already {result['hash']}", ())
        return ("Plan accepted", f"{result['from']} became {result['hash']}", ())
    if command == "emit":
        where = result.get("path", "returned inline")
        return ("Layout emitted", f"{len(result['functions'])} functions of plan {result['hash']}: {where}", ())
    return (command, "", ())


def _render(command: str, checks: bool = False):
    """The Activity callback of one command. `checks` marks a command whose answer is a verdict."""

    def render(*, phase: str, result: Any = None, **_: Any):
        if phase != "success" or not isinstance(result, dict):
            return None
        if not result.get("ok"):
            error = result.get("error", {})
            extra = {"verdict": "failed"} if checks else {}
            return vis.ActivityPresentation(
                f"Refused: {error.get('code')}",
                str(error.get("message")),
                content=(vis.ActivityText(str(error.get("hint"))),),
                **extra,
            )
        headline, summary, content = _summaries(command, result)
        extra = {"verdict": "passed"} if checks else {}
        return vis.ActivityPresentation(headline, summary, content=content, **extra)

    return render


def _activity(command: str, label: str, slow: bool = False, checks: bool = False) -> Any:
    return vis.Activity(label=label, show_start=slow, render=_render(command, checks))


# ---- tools -------------------------------------------------------------------

PlanPath = Annotated[str, "Path of a plan file (EDN)"]


class Descent:
    """Search for the data layout that serves the access patterns of a nested map fastest."""

    @vis.method(tag="observation", activity=_activity("facts", "Read access facts"))
    def facts(
        self,
        paths: Annotated[list[str], "Source directories of the Clojure code to read"],
        limit: Annotated[int, "Sites listed for each path shape; 0 gives counts only"] = 5,
        min_depth: Annotated[int, "Fewest computed levels (`?` and `*`) of a shape that is shown"] = 2,
        writes: Annotated[bool, "Also show the sites that write (assoc, update, ...)"] = False,
    ) -> dict[str, Any]:
        """Report where a code base reads and scans nested maps, grouped by path shape.

        In a shape, `?` is a key that the code computes (a :key level), `*` a scan of every key
        (a :scan level) and anything else a constant. A lookup inside the function of a scan
        continues the shape of the scan. File names are relative to `root`. `hidden` counts what
        `min_depth` and `writes` leave out. Nothing is changed.
        """
        return run_cli(
            "facts",
            *[_path(p) for p in paths],
            *_flags(limit=limit, min_depth=min_depth, writes="true" if writes else None),
        )

    @vis.method(tag="observation", activity=_activity("check", "Check plan"))
    def check(self, plan: PlanPath) -> dict[str, Any]:
        """Validate a plan file and return its hash, task and warnings, or its problems."""
        return run_cli("check", _path(plan))

    @vis.method(tag="observation", activity=_activity("status", "Read plan status"))
    def status(self, plan: PlanPath) -> dict[str, Any]:
        """Describe a plan: the order that serves each pattern, how to call each emitted function,
        the state of the emitted file and the last journal records.

        A stale emitted file has `emitted.cause` (`plan-changed`, `plan-renamed`, `file-edited`,
        `emitter-changed`, or `file-or-emitter-changed` for a journal of an older version) and
        `emitted.fix`. Follow the fix: for `plan-changed` it is not `accept`."""
        return run_cli("status", _path(plan))

    @vis.method(tag="observation", activity=_activity("journal", "Read journal"))
    def journal(
        self,
        plan: PlanPath,
        last: Annotated[int | None, "Only the newest records; the default is all"] = None,
    ) -> dict[str, Any]:
        """Return the journal of a plan as `entries`: every evaluation with its verdict, costs, bytes
        and the ns of each pattern, and every acceptance. `unreadable_lines` counts lines that are
        no record."""
        return run_cli("journal", _path(plan), *_flags(last=last))

    @vis.method(tag="observation", activity=_activity("measure", "Measure plans", slow=True))
    def measure(
        self,
        plans: Annotated[list[str], "Plan files of one task; one is enough"],
        rounds: Annotated[int | None, "Timed rounds for each pattern; the default is 15"] = None,
        warm_ms: Annotated[int | None, "Warm-up for each pattern in milliseconds; the default is 150"] = None,
    ) -> dict[str, Any]:
        """Gate plans of one task and time them on the same data, with no verdict and no journal record.

        Each plan gets its cost, estimated bytes, build time and the ns of each pattern. The
        numbers compare inside one call only. Use it for the table of a report; `evaluate` decides.
        """
        return run_cli("measure", *[_path(p) for p in plans], *_flags(rounds=rounds, warm_ms=warm_ms))

    @vis.method(tag="verification", activity=_activity("verify", "Run the gate", slow=True, checks=True))
    def verify(
        self,
        plan: PlanPath,
        source: Annotated[str | None, "A trusted Clojure file to execute and check in place of fresh output"] = None,
    ) -> dict[str, Any]:
        """Compile the code of a plan and compare every pattern with the reference model.

        Passing `source` executes that file. Use only trusted code.

        `ok` is false, with `gate.mismatches`, when the code disagrees with the model or compiles
        with a warning.
        """
        return run_cli("verify", _path(plan), *_flags(source=_path(source) if source else None))

    @vis.method(tag="mutation", activity=_activity("neighbors", "List neighbor plans"))
    def neighbors(
        self,
        plan: PlanPath,
        out_dir: Annotated[str | None, "Directory that receives one candidate file per neighbor"] = None,
    ) -> dict[str, Any]:
        """List every plan one decision away, with its move and the reason for it.

        With `out_dir`, each neighbor is written as `<task-hash>/<layout-hash>.edn` and its `path` is returned. The
        neighbors are a starting set: any valid plan of the same task can be a candidate.
        """
        return run_cli("neighbors", _path(plan), *_flags(out_dir=_path(out_dir) if out_dir else None))

    @vis.method(tag="verification", activity=_activity("evaluate", "Measure candidate", slow=True))
    def evaluate(
        self,
        candidate: Annotated[str, "Path of the candidate plan"],
        against: Annotated[str, "Path of the plan that the candidate would replace"],
        rounds: Annotated[int | None, "Timed rounds for each pattern; the default is 15"] = None,
        warm_ms: Annotated[int | None, "Warm-up for each pattern in milliseconds; the default is 150"] = None,
    ) -> dict[str, Any]:
        """Gate both plans, time them on the same data and return the verdict.

        `verdict` is `better`, `worse` or `no-change`. A `better` verdict was confirmed on holdout
        data. `no-change` means that the difference is inside the noise or below :min-gain: it says
        nothing about direction. `sensitivity` tells whether other :weights can reverse the verdict.
        The measurement is appended to `<against>.journal`, where `accept` looks for it.
        """
        return run_cli(
            "evaluate", _path(candidate), "--against", _path(against), *_flags(rounds=rounds, warm_ms=warm_ms)
        )

    @vis.method(tag="mutation", activity=_activity("descend", "Search neighbor plans", slow=True))
    def descend(
        self,
        plan: PlanPath,
        max_steps: Annotated[int, "The most moves to take"] = 4,
        out: Annotated[str | None, "File that receives the final candidate"] = None,
        rounds: Annotated[int | None, "Timed rounds for each pattern"] = None,
        warm_ms: Annotated[int | None, "Warm-up for each pattern in milliseconds"] = None,
    ) -> dict[str, Any]:
        """Take the best measured neighbor move, again and again, until none is better.

        The plan file is not changed. The result names a `candidate` file to pass to `accept`.
        Each step has the cost, the bytes and the ns of each pattern for the plan and every
        neighbor; a chosen plan is written under `candidates/` beside the plan and named in
        `chosen.path`. `measured` compares the start and the final plan in one run. This is the
        mechanical baseline of the search: a candidate that needs two decisions at once is not found.
        """
        return run_cli(
            "descend",
            _path(plan),
            *_flags(max_steps=max_steps, out=_path(out) if out else None, rounds=rounds, warm_ms=warm_ms),
        )

    @vis.method(tag="mutation", activity=_activity("accept", "Accept candidate"))
    def accept(
        self,
        candidate: Annotated[str, "Path of the candidate plan"],
        into: Annotated[str, "Path of the plan that takes the candidate's orders"],
        force: Annotated[str | None, "Reason to accept without a better verdict; written to the journal"] = None,
    ) -> dict[str, Any]:
        """Move the layout decisions of a candidate into the plan and emit the plan's source file.

        Refused with `not_measured` unless the journal holds a `better` verdict for these two
        plans under the current objective. The gate runs again before anything is written. A retry
        finishes interrupted publication before starting another acceptance.
        """
        return run_cli("accept", _path(candidate), "--into", _path(into), *_flags(force=force))

    @vis.method(tag="mutation", activity=_activity("emit", "Emit layout source"))
    def emit(
        self,
        plan: PlanPath,
        out: Annotated[str | None, "Optional preview file to write; must not be the configured :emit :path"] = None,
    ) -> dict[str, Any]:
        """Gate a plan, then write the Clojure namespace that implements it.

        With no `out`, the source is returned as `source`. Only `accept` writes the configured path.
        """
        return run_cli("emit", _path(plan), *_flags(out=_path(out) if out else None))


vis.register_extension(
    vis.Extension(
        name="layout-descent",
        description="Measured search for the data layout of nested-map access patterns in Clojure code.",
        alias="layout-descent",
        prompt=PROMPT,
        symbols=[vis.Symbol(Descent(), name="descent")],
    )
)
