"""Tests of the Vis extension entry file. Run: python3 test/extension_test.py

The Vis SDK is replaced by a stub, so the tests need only Python and, for the last one, the
`clojure` command.
"""

import importlib.util
import json
import shutil
import sys
import types
import unittest
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
ENTRY = ROOT / ".vis" / "extensions" / "layout_descent_tools.py"


def stub_sdk():
    """A module that records what the entry file declares."""
    sdk = types.ModuleType("blockether.vis.extension")
    sdk.registered = []

    def record(kind):
        def make(*args, **kwargs):
            return types.SimpleNamespace(kind=kind, args=args, **kwargs)

        return make

    for kind in ("Extension", "Symbol", "Activity", "ActivityPresentation", "ActivityText", "ActivityTable"):
        setattr(sdk, kind, record(kind))

    def method(**meta):
        def mark(fn):
            fn.vis_meta = meta
            return fn

        return mark

    sdk.method = method
    sdk.register_extension = sdk.registered.append
    return sdk


def load_entry():
    sdk = stub_sdk()
    packages = {"blockether": types.ModuleType("blockether"), "blockether.vis": types.ModuleType("blockether.vis")}
    with mock.patch.dict(sys.modules, {**packages, "blockether.vis.extension": sdk}):
        spec = importlib.util.spec_from_file_location("layout_descent_tools_under_test", ENTRY)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
    return module, sdk


def finished(answer, code):
    return types.SimpleNamespace(stdout=json.dumps(answer), stderr="", returncode=code)


class ExtensionTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module, cls.sdk = load_entry()
        cls.descent = cls.sdk.registered[0].symbols[0].args[0]

    def test_registers_one_namespace(self):
        (extension,) = self.sdk.registered
        self.assertEqual("layout-descent", extension.name)
        self.assertEqual("layout-descent", extension.alias)
        self.assertEqual(["descent"], [s.name for s in extension.symbols])
        self.assertIn("accept", extension.prompt)
        for rule in ("not the command line in a shell", "no claim of direction", "numbers of one run", ":notes",
                     "pass `save`", "Do not\n  parse the EDN files", "Do not call a result an optimum",
                     "range of the\n  estimate", "clojurescript-sources"):
            self.assertIn(rule, extension.prompt)

    def test_prompt_requires_lifecycle_evidence_for_refactors(self):
        (extension,) = self.sdk.registered
        prompt = extension.prompt
        for rule in ("producer as well as the readers", "a direct builder",
                     "both creation of the old layout and conversion", "across rebuilds",
                     "build plus zero, one, and several reads", "reads per build are unknown",
                     "A `better` layout verdict is not a refactor verdict",
                     "avoid single-letter names", "invariants and compatibility choices"):
            self.assertIn(rule, prompt)

    def test_every_tool_has_a_tag_an_activity_and_a_docstring(self):
        tools = [n for n in dir(self.descent) if not n.startswith("_")]
        self.assertEqual(
            ["accept", "check", "descend", "emit", "evaluate", "facts", "flow", "journal", "measure", "neighbors",
             "report", "schema", "status", "verify"],
            tools,
        )
        for name in tools:
            meta = getattr(self.descent, name).vis_meta
            self.assertIn(meta["tag"], {"observation", "mutation", "verification"}, name)
            self.assertTrue(meta["activity"].label[0].isupper(), name)
            self.assertTrue(getattr(self.descent, name).__doc__, name)
        for name in ("accept", "emit", "descend", "neighbors"):
            self.assertEqual("mutation", getattr(self.descent, name).vis_meta["tag"], name)

    def test_arguments_become_one_command_line(self):
        with mock.patch.object(self.module.subprocess, "run", return_value=finished({"ok": True}, 0)) as run:
            self.descent.evaluate("/tmp/c.edn", against="plans/p.edn", rounds=7)
        command = run.call_args.args[0]
        self.assertEqual(["clojure", "-M:cli", "evaluate", "/tmp/c.edn"], command[:4])
        self.assertEqual(["--against", str(ROOT / "plans/p.edn"), "--rounds", "7"], command[4:])
        self.assertEqual(ROOT, run.call_args.kwargs["cwd"])
        with mock.patch.object(self.module.subprocess, "run", return_value=finished({"ok": True}, 0)) as run:
            self.descent.facts(["/src"], limit=0, writes=True)
            self.descent.facts(["/src"])
            self.descent.measure(["/a.edn", "plans/b.edn"], rounds=5)
            self.descent.journal("/a.edn", last=3)
            self.descent.report("/a.edn")
            self.descent.descend("/a.edn", save="out/trace.json")
            self.descent.flow("src", "orders/settle", save="out/flow.json")
            self.descent.flow("/src", "shared/act", lang="cljs")
            self.descent.schema("src", entity="e2", diagram="out/model.md")
        commands = [call.args[0][2:] for call in run.call_args_list]
        self.assertEqual(["facts", "/src", "--limit", "0", "--min-depth", "2", "--writes", "true"], commands[0])
        self.assertEqual(["facts", "/src", "--limit", "5", "--min-depth", "2"], commands[1])
        self.assertEqual(["measure", "/a.edn", str(ROOT / "plans/b.edn"), "--rounds", "5"], commands[2])
        self.assertEqual(["journal", "/a.edn", "--last", "3"], commands[3])
        self.assertEqual(["report", "/a.edn"], commands[4])
        self.assertEqual(
            ["descend", "/a.edn", "--max-steps", "4", "--save", str(ROOT / "out/trace.json")], commands[5])

        self.assertEqual(["flow", str(ROOT / "src"), "--operation", "orders/settle", "--save", str(ROOT / "out/flow.json")],
                         commands[6])
        self.assertEqual(["flow", "/src", "--operation", "shared/act", "--lang", "cljs"], commands[7])
        self.assertEqual(["schema", str(ROOT / "src"), "--limit", "30", "--entity", "e2", "--sites", "3",
                          "--diagram", str(ROOT / "out/model.md")], commands[8])

    def test_a_refusal_is_an_answer_and_a_usage_error_is_an_exception(self):
        refusal = {"ok": False, "error": {"code": "not_measured", "message": "m", "hint": "h"}}
        with mock.patch.object(self.module.subprocess, "run", return_value=finished(refusal, 1)):
            self.assertEqual(refusal, self.descent.accept("/c.edn", into="/p.edn"))
        usage = {"ok": False, "error": {"code": "usage", "message": "m", "hint": "h"}}
        with mock.patch.object(self.module.subprocess, "run", return_value=finished(usage, 2)):
            with self.assertRaises(self.module.DescentError):
                self.descent.check("/p.edn")
        silent = types.SimpleNamespace(stdout="", stderr="no clojure", returncode=127)
        with mock.patch.object(self.module.subprocess, "run", return_value=silent):
            with self.assertRaisesRegex(self.module.DescentError, "no clojure"):
                self.descent.check("/p.edn")

    def test_activity_tells_the_verdict(self):
        verify = self.descent.verify.vis_meta["activity"].render
        self.assertIsNone(verify(phase="start", args=(), kwargs={}, result=None, error=None))
        passed = verify(phase="success", args=(), kwargs={}, error=None,
                        result={"ok": True, "gate": {"checks": 960, "hash": "abc"}})
        self.assertEqual("passed", passed.verdict)
        failed = verify(phase="success", args=(), kwargs={}, error=None,
                        result={"ok": False, "error": {"code": "gate_failed", "message": "m", "hint": "h"}})
        self.assertEqual("failed", failed.verdict)
        evaluate = self.descent.evaluate.vis_meta["activity"].render
        shown = evaluate(phase="success", args=(), kwargs={}, error=None, result={
            "ok": True, "verdict": "better", "gain": 0.5,
            "baseline": {"hash": "a", "patterns": [{"pattern": "p", "ns": 10.0, "weight": 1}]},
            "candidate": {"hash": "b", "patterns": [{"pattern": "p", "ns": 5.0, "weight": 1}]}})
        self.assertEqual(("Candidate is better", "Cost -50.0 %, b against a"), shown.args)
        self.assertFalse(hasattr(shown, "verdict"))
        measure = self.descent.measure.vis_meta["activity"].render
        table = measure(phase="success", args=(), kwargs={}, error=None, result={
            "ok": True, "task": "t", "entries": 9, "plans": [
                {"hash": "a", "cost": 7.25, "bytes": 64, "patterns": [{"pattern": "p", "ns": 7.25}]}]})
        self.assertEqual(("Plans measured", "1 plans of task t on 9 entries"), table.args)
        self.assertEqual((["Hash", "Cost", "Bytes", "p ns"], [["a", "7.2", "64", "7.2"]]), table.content[0].args)
        descend = self.descent.descend.vis_meta["activity"].render
        kept = descend(phase="success", args=(), kwargs={}, error=None, result={
            "ok": True, "verdict": "no-change", "start": "a", "final": "a", "steps": [{"chosen": None}],
            "reach": {"timed": 2}, "stopped": "No candidate is better."})
        self.assertEqual(
            ("Descent is no-change", "0 steps from a to a, 2 candidates timed. No candidate is better."), kept.args)
        facts = self.descent.facts.vis_meta["activity"].render
        empty = facts(phase="success", args=(), kwargs={}, error=None, result={
            "ok": True, "site_count": 3, "files": 1, "shapes": [], "hidden": {"shapes": 2},
            "warnings": [{"code": "clojurescript-sources", "message": "JVM only."}], "hint": "Lower min-depth."})
        self.assertEqual(["JVM only.", "Lower min-depth."], [c.args[0] for c in empty.content])
        schema = self.descent.schema.vis_meta["activity"].render
        model = schema(phase="success", args=(), kwargs={}, error=None, result={
            "ok": True, "functions": 4, "files": 2, "truncated": ["a/b"], "untraced": [], "links": [{}],
            "hidden": {"entities": 3, "loose_records": 1}, "diagram": "/tmp/m.md",
            "entities": [{"id": "e1", "name": "order", "records": 2, "keys": [{"key": ":id"}, {"key": ":total"}]}]})
        self.assertEqual(
            ("Data model", "1 entities shown, 3 hidden, 1 links, 4 functions in 2 files, 1 traces incomplete"),
            model.args)
        self.assertEqual((["Id", "Entity", "Records", "Keys", "First keys"], [["e1", "order", "2", "2", ":id :total"]]),
                         model.content[0].args)
        self.assertEqual("Diagram: /tmp/m.md", model.content[1].args[0])

    @unittest.skipUnless(shutil.which("clojure"), "needs the clojure command")
    def test_the_real_command_line_answers(self):
        answer = self.descent.check("examples/risk/plan.edn")
        self.assertTrue(answer["ok"])
        self.assertEqual("e471a80f71f4", answer["hash"])
        status = self.descent.status("examples/risk/plan.edn")
        render = self.descent.status.vis_meta["activity"].render
        self.assertEqual("Plan risk e471a80f71f4",
                         render(phase="success", args=(), kwargs={}, result=status, error=None).args[0])
        model = self.descent.schema("test/fixtures/forms")
        self.assertEqual(["order (:id)", "ticket"], [e["name"] for e in model["entities"][:2]])
        shown = self.descent.schema.vis_meta["activity"].render(
            phase="success", args=(), kwargs={}, result=model, error=None)
        self.assertEqual("10 entities shown, 0 hidden, 1 links, 13 functions in 1 files", shown.args[1])


if __name__ == "__main__":
    unittest.main()
