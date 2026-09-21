#!/usr/bin/env bash
#
# tag-submodule-updates.sh
#
# For every distinct submodule commit referenced by a commit in the
# master history of this repo that updates a git submodule (its gitlink
# differs from all parents), create a lightweight tag in the submodule
# repo pointing at that submodule commit, named after the short hash of
# the oldest such commit. New tags are pushed to the submodule's origin.
#
# This keeps the submodule commits alive against GC: after the submodules are
# is rebased, the old commits in `nlvm` still point at submodule
# commits that are unreachable in the submodule repo and would be
# pruned by `git gc` there.

set -euo pipefail

# Tag the submodule commits referenced by every commit that updated
# the gitlink at the given path(s). $1 is the submodule's current
# path (used to locate its git dir); any further args are historical
# paths the gitlink lived at before a rename.
tag_submodule_updates() {
  local sub_path="$1"
  shift
  local -a paths=("$sub_path" "$@")

  # The submodule's real git dir (e.g. .git/modules/lib/nim),
  # regardless of how the modules dir is named.
  local sub_git_dir
  sub_git_dir="$(git -C "$sub_path" rev-parse --absolute-git-dir)"

  local created=0 skipped=0 missing=0
  local c sub tag

  while read -r sub c; do
    tag="nlvm-master-$(git rev-parse --short "$c")"

    # Skip if any nlvm-master-* tag already points at this submodule
    # commit (re-run, or a duplicate left behind by an earlier run).
    if git -C "$sub_git_dir" tag -l 'nlvm-master-*' --points-at "$sub" | grep -q .; then
      skipped=$((skipped + 1))
      continue
    fi

    if ! git -C "$sub_git_dir" cat-file -e "$sub" 2>/dev/null; then
      missing=$((missing + 1))
      echo "WARNING: submodule commit $sub (referenced by nlvm $c) not present in $sub_git_dir — cannot tag" >&2
      continue
    fi

    git -C "$sub_git_dir" tag "$tag" "$sub"
    created=$((created + 1))
    echo "tagged $tag -> $sub (nlvm $c)"
  done < <(
    # Emit "sub-sha oldest-nlvm-commit" for every distinct submodule
    # commit referenced by a commit in master's history whose gitlink
    # at one of the given paths differs from all its parents (or which
    # introduces the submodule). Keep only the oldest nlvm commit per
    # submodule commit, so each one is tagged exactly once.
    git log master --pretty='format:%H %ct' -- "${paths[@]}" | sort -u |
    while read -r c date; do
      sub="$(git ls-tree "$c" -- "${paths[@]}" 2>/dev/null | awk '$2=="commit"{print $3; exit}')" || sub=""
      [ -z "$sub" ] && continue
      echo "$sub $date $c"
    done |
    sort -k1,1 -k2,2n -k3,3 | awk '!seen[$1]++ {print $1" "$3}'
  )

  echo "$sub_path: $created created, $skipped already tagged, $missing missing"

  if [ "$created" -gt 0 ]; then
    git -C "$sub_git_dir" push origin 'refs/tags/nlvm-master-*'
  fi
}

# The Nim gitlink used to live at "Nim" before it was moved to
# "lib/nim"; scan both paths.
tag_submodule_updates lib/nim Nim
tag_submodule_updates llvm/llvm-project
