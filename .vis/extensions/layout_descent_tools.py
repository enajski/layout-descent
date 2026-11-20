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
2. Write a plan with one order. `{ROOT}/examples/risk/plan.edn` is a model; `{ROOT}/README.md` has the keys.
3. `descent.check(plan)`, then `descent.verify(plan)`.
4. Get candidates from `descent.neighbors(plan, out_dir)`, or write one. A candidate keeps the task
   and changes only :orders and :serve.
5. `descent.evaluate(candidate, against=plan)`. The verdict is `better`, `worse` or `no-change`.
6. `descent.accept(candidate, into=plan)` only after a `better` verdict.

Rules:
- `accept` is the only writer of an accepted plan and of its emitted file. Do not edit either.
- Do not change :workload or :objective to make a candidate win. They are the user's statement of
  the task: ask the user.
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
        return (
            "Access facts",
            f"{result['site_count']} sites in {result['files']} files, {len(shapes)} path shapes",
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
        rows = [[c["hash"], c["move"]["op"], _percent(c["gain"]), c["why"]] for c in taken]
        return (
            f"Descent is {result['verdict']}",
            f"{len(taken)} steps from {result['start']} to {result['final']}. {result['stopped']}",
            (vis.ActivityTable(["Hash", "Move", "Gain", "Why"], rows),) if rows else (),
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
        limit: Annotated[int, "Sites listed for each path shape"] = 5,
    ) -> dict[str, Any]:
        """Report where a code base reads, writes and scans nested maps, grouped by path shape.

        In a shape, `?` is a key that the code computes (a :key level), `*` a scan of every key
        (a :scan level) and anything else a constant. Nothing is changed.
        """
        return run_cli("facts", *[_path(p) for p in paths], *_flags(limit=limit))

    @vis.method(tag="observation", activity=_activity("check", "Check plan"))
    def check(self, plan: PlanPath) -> dict[str, Any]:
        """Validate a plan file and return its hash, task and warnings, or its problems."""
        return run_cli("check", _path(plan))

    @vis.method(tag="observation", activity=_activity("status", "Read plan status"))
    def status(self, plan: PlanPath) -> dict[str, Any]:
        """Describe a plan: the order that serves each pattern, how to call each emitted function,
        the state of the emitted file and the last journal records."""
        return run_cli("status", _path(plan))

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
        data. The measurement is appended to `<against>.journal`, where `accept` looks for it.
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
        This is the mechanical baseline of the search: a candidate that needs two decisions at
        once is not found.
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
