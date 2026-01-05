#!/usr/bin/env bash

# This script mirrors selected sub‑folders from the upstream llvm‑project
# into the local `llvm-project` directory that ships with nlvm.
#
# Usage:
#   ./update-llvm.sh

set -euo pipefail

# The sub-projects we'll preserve
DIRS="cmake lld llvm libunwind third-party"

# Resolve absolute paths
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
SRC_DIR=$(cd "$SCRIPT_DIR/../../llvm-project" && pwd)
DST_DIR="$SCRIPT_DIR/llvm-project"

VERSION=$(cat llvm.version)
COMMIT=$(git -C "$SRC_DIR" rev-parse HEAD)

mkdir -p "$DST_DIR"

# Remove existing content, if any
for f in $DIRS; do
  rm -rf "$DST_DIR/$f"
done

git -C "$SRC_DIR" checkout llvmorg-$VERSION

# Copy all tracked files, excluding tests/benchmarks/other large cruft - tar is
# used as an easy hack to preserve timestamps etc
git -C "$SRC_DIR" ls-files $DIRS | \
  grep -v -e '/test/' -e '/benchmarks/' -e 'third-party/benchmark/' -e 'third-party/unittest/' | \
  tar -C "$SRC_DIR" -cT - |
	tar -C "$DST_DIR" -x

git -C $DST_DIR add -A
git -C $DST_DIR commit -m "llvm $VERSION ($COMMIT)" -m "Includes: $DIRS"

