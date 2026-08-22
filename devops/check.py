#!/usr/bin/env python3
"""Structural checks standing in for the compiler.

Three passes, each aimed at a mistake real work has produced here:
  1. module header must match its path
  2. a qualified name must have its module imported  (a whole block was once
     deleted, and nothing noticed until the app failed to start)
  3. a qualified name must be exposed by that module
  4. no non-English text outside string literals, in Elm and in TypeScript

Pass 4 is the only one a compiler cannot do. Passes 1-3 duplicate `elm make`
for modules reachable from Main, and still earn their place for modules that
are not yet reachable, which a port in progress always has.
"""
import re
import sys
import pathlib

SRC = pathlib.Path("src")
RUNTIME = pathlib.Path("runtime")


def strip_code(text):
    text = re.sub(r'"""(.*?)"""', '""', text, flags=re.S)
    text = re.sub(r'"(?:[^"\\\n]|\\.)*"', '""', text)
    text = re.sub(r"\{-.*?-\}", " ", text, flags=re.S)
    return re.sub(r"--[^\n]*", " ", text)


def balanced_after(text, start):
    """Text inside the parentheses beginning at or after `start`."""
    open_at = text.find("(", start)
    if open_at < 0:
        return ""
    depth = 0
    for index in range(open_at, len(text)):
        if text[index] == "(":
            depth += 1
        elif text[index] == ")":
            depth -= 1
            if depth == 0:
                return text[open_at + 1 : index]
    return ""


def variants_of(text, type_name):
    match = re.search(
        r"^type\s+" + re.escape(type_name) + r"\b(.*?)(?=^\S|\Z)", text, re.S | re.M
    )
    block = match.group(1) if match else ""
    return set(re.findall(r"(?:=|\|)\s*([A-Z]\w*)", block))


VIETNAMESE = r"[\u00e0\u00e1\u1ea3\u00e3\u1ea1\u0103\u00e2\u0111\u00e8\u00e9\u1ebb\u1ebd\u1eb9\u00ea\u00ec\u00ed\u1ec9\u0129\u1ecb\u00f2\u00f3\u1ecf\u00f5\u1ecd\u00f4\u01a1\u00f9\u00fa\u1ee7\u0169\u1ee5\u01b0\u1ef3\u00fd\u1ef7\u1ef9\u1ef5]"

BUILT_IN = {
    "Basics", "List", "String", "Maybe", "Result", "Char", "Tuple", "Debug",
    "Platform", "Cmd", "Sub", "Set", "Dict", "Array", "Bitwise", "Process",
}

modules = {}
problems = []

for path in sorted(SRC.rglob("*.elm")):
    text = path.read_text()
    header = re.match(r"(?:port\s+)?module\s+([\w.]+)\s+exposing", text)
    if not header:
        problems.append(f"{path}: no module header")
        continue

    name = header.group(1)
    expected = str(path.relative_to(SRC)).replace("/", ".")[:-4]
    if name != expected:
        problems.append(f"{path}: module is {name}, path says {expected}")

    body = balanced_after(text, header.end())
    names = set()
    for item in body.split(","):
        item = item.strip()
        base = re.match(r"([A-Za-z_]\w*)", item)
        if not base:
            continue
        names.add(base.group(1))
        if "(..)" in item:
            names |= variants_of(text, base.group(1))

    modules[name] = {"path": path, "text": text, "exposes": names}

for name, module in modules.items():
    code = strip_code(module["text"])
    code = "\n".join(
        line
        for line in code.splitlines()
        if not line.lstrip().startswith("import ")
        and not line.lstrip().startswith("module ")
        and not line.lstrip().startswith("port module ")
    )

    imported = {}
    for line in module["text"].splitlines():
        line_match = re.match(r"import\s+([\w.]+)(?:\s+as\s+(\w+))?", line)
        if line_match:
            target = line_match.group(1)
            imported[line_match.group(2) or target.split(".")[-1]] = target
            imported[target] = target

    for qualified in sorted(set(re.findall(r"\b([A-Z][\w]*(?:\.[A-Z][\w]*)*)\.([a-zA-Z]\w*)", code))):
        alias, member = qualified
        root = alias.split(".")[0]
        if alias in imported:
            target = imported[alias]
        elif root in imported or root in BUILT_IN:
            continue
        else:
            problems.append(f"{module['path']}: uses {alias}. but never imports it")
            continue

        if target in modules and member not in modules[target]["exposes"]:
            problems.append(f"{module['path']}: {alias}.{member} is not exposed by {target}")

    outside_strings = re.sub(r'"""(.*?)"""', '""', module["text"], flags=re.S)
    outside_strings = re.sub(r'"(?:[^"\\\n]|\\.)*"', '""', outside_strings)
    for line_no, line in enumerate(outside_strings.splitlines(), 1):
        if re.search(VIETNAMESE, line, re.I):
            problems.append(
                f"{module['path']}:{line_no}: non-English text outside a string literal"
            )

def strip_ts_strings(text):
    """Blank every TypeScript string so only code and comments remain."""
    text = re.sub(r"`(?:[^`\\]|\\.)*`", "``", text, flags=re.S)
    text = re.sub(r'"(?:[^"\\\n]|\\.)*"', '""', text)
    return re.sub(r"'(?:[^'\\\n]|\\.)*'", "''", text)


typescript_files = sorted(RUNTIME.rglob("*.ts")) if RUNTIME.is_dir() else []
for path in typescript_files:
    outside_strings = strip_ts_strings(path.read_text())
    for line_no, line in enumerate(outside_strings.splitlines(), 1):
        if re.search(VIETNAMESE, line, re.I):
            problems.append(
                f"{path}:{line_no}: non-English text outside a string literal"
            )

print(f"modules checked: {len(modules)} elm, {len(typescript_files)} ts")
if problems:
    print(f"problems: {len(problems)}")
    for problem in problems:
        print("  " + problem)
    sys.exit(1)
print("no structural problems")
