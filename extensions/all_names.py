#!/usr/bin/env python
"""Inspect modules and obtain object paths within the virtual environment."""

import importlib.util
import inspect
import logging
import pkgutil
import sys
import threading
import time
from argparse import ArgumentParser
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from contextlib import contextmanager, redirect_stdout
from importlib import import_module, metadata
from pathlib import Path
from sys import stdlib_module_names
from typing import Iterable, List, Set

logger = logging.getLogger(__name__)


def write(items: List[str], notfrom: bool = False) -> str:
    """Get the full module path."""
    return ".".join(items)


thread_local = threading.local()


@contextmanager
def thread_safe_redirect_stdout(target):
    if not hasattr(thread_local, "stdout"):
        thread_local.stdout = sys.__stdout__
    orig_stdout = thread_local.stdout
    thread_local.stdout = target
    sys.stdout = thread_local.stdout
    yield
    thread_local.stdout = orig_stdout
    sys.stdout = orig_stdout


def import_module_thread(parts: List[str], timeout: float = 1):
    """Run module import with timeout."""
    result = [None]

    def _impmod(parts: List[str]) -> None:
        try:
            with thread_safe_redirect_stdout(sys.stderr):
                # Some modules (usually tests) run time-consuming
                # tasks or prompt for use input. We likely don't need
                # them so let them fail with timeout.
                module = import_module(".".join(parts))
        except BaseException as exc:
            logger.info(f"Not a module or package: {parts}")
            result[0] = exc
        else:
            result[0] = module

    th = threading.Thread(target=_impmod, args=(parts,), daemon=True)
    th.start()
    th.join(timeout=timeout)
    if th.is_alive():
        result[0] = TimeoutError

    return result[0]


def _find_importables(parts: List[str]) -> List[str]:
    logger.debug(f"_find_importables({parts})...")
    result: List[str] = []

    with ThreadPoolExecutor() as p:
        f = p.submit(import_module_thread, parts)
        module = f.result()

    if isinstance(module, BaseException):
        return result

    result.append(write(parts, notfrom=True))

    seen = set()

    if hasattr(module, "__path__"):
        for importer, name, ispkg in pkgutil.iter_modules(module.__path__):
            logger.debug(f"From __path__: {parts} {name} {ispkg}")
            if name.startswith("_"):
                continue

            result.extend(_find_importables(parts + [name]))
            seen.add(name)

    try:
        hasattr(module, "__all__")
    except Exception:
        logger.debug("Skipping due to lazy module loading error? %s", module)
        return result

    if hasattr(module, "__all__"):
        for name in (name for name in module.__all__ if name not in seen):
            name = name.__name__ if not isinstance(name, str) else name
            logger.debug(f"From __all__: {parts} {name}")
            result.append(write(parts + [name]))

    else:
        for name in (name for name in dir(module) if name not in seen):
            logger.debug(f"From dir(module): {parts} {name}")
            if name.startswith("_"):
                continue

            obj = getattr(module, name)

            # If this is a module and not a package, then most likely
            # this is an import of stdlib or third-party
            # module/package, which we don't want to process.
            if inspect.ismodule(obj):
                continue

            assoc_module = obj.__module__ if hasattr(obj, "__module__") else None
            logger.debug(
                f"Found {name} associated with {assoc_module} in {module.__name__}"
            )

            # If the imported object is not associated with the
            # currently inspected module/package but with a stdlib or
            # third-party module/package, don't want this exposed.
            if assoc_module != module.__name__:
                continue

            result.append(write(parts + [name]))

    return result


def _process_packages(iterable: Iterable) -> None:
    """Iterate over packages and dump module paths to stdout."""
    result: List[str] = []

    futures = []
    for package in iterable:
        with ThreadPoolExecutor(max_workers=1000) as p:
            f = p.submit(_find_importables, [package])
            futures.append(f)

    for future in as_completed(futures):
        result.extend(future.result())

    for i in result:
        print(i)


def _dump_module_paths(packages: Set[str], excludes: Set[str]) -> None:
    # By default, exclude modules that cause issues on loading, e.g.,
    # launching GUI, prompting for user input.
    excludes = excludes or set(["antigravity", "idlelib", "test", "this"])
    gen = (
        package
        for package in (
            packages or (modname for finder, modname, ispkg in pkgutil.iter_modules())
        )
        if package not in excludes
    )
    _process_packages(gen)


if __name__ == "__main__":
    p = ArgumentParser()
    p.add_argument("package", nargs="*", help="Python module(s) to include.")
    p.add_argument("--exclude", "-e", action="append", help="Python module to exclude")
    p.add_argument("--init", "-i", type=str, help="Init Python script")
    args = p.parse_args()

    if args.init:
        spec = importlib.util.spec_from_file_location("init", args.init)
        mod = importlib.util.module_from_spec(spec)
        sys.modules["init"] = mod
        spec.loader.exec_module(mod)

    packages = set(args.package or [])
    excludes = set(args.exclude or [])
    _dump_module_paths(packages, excludes)
