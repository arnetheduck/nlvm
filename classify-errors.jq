#!/usr/bin/env jq -f
# classify-errors.jq — classification rules for nlvm test failures.
#
# Usage:
#   cat input.json          | jq -f classify-errors.jq
#   jq -s '[.[]|select(.result!="reSuccess")]' *.json | jq -f classify-errors.jq
#
# Input:  JSON array of test-failure objects (each with "given"/"expected")
# Output: Same array with "classification" and "limitation" added to each entry.
#         Duplicate entries sharing the same ".name" are merged — string fields
#         (given, expected, classification, result) are joined with a pipe
#         separator so no information is lost.

# ---------------------------------------------------------------------------
# Classification rules  (regex → {category, limitation})
# limitation = true means it's a known limitation, false = a bug.
# ---------------------------------------------------------------------------
def rules:
  [
    # ── known limitations (limitation = true) ──────────────────────────
    { pattern: "emit pragma not supported",       category: "Pragmas/{.emit.}",    limitation: true  },
    { pattern: "header pragma|header:",           category: "Pragmas/{.header.}", limitation: true  },
    { pattern: "asm pragma",                      category: "Pragmas/{.asm.}",    limitation: true  },
    { pattern: "importcpp|importobjc",            category: "Pragmas/C\\+/ObjC",  limitation: true  },
    { pattern: "dynlib|dynamic library",          category: "Pragmas/{.dynlib.}", limitation: true  },
    { pattern: "clangMain|clang:",                category: "C Code Gen",         limitation: true  },

    # ── nlvm bugs (limitation = false) ────────────────────────────────
    { pattern: "llvm::.*assertion",               category: "LLVM IR",            limitation: false },
    { pattern: "reNimcCrash.*internal",           category: "Internal Error",     limitation: false },

    { pattern: "undefined symbol: nimrtl_",       category: "Linker/nimrtl",      limitation: false },
    { pattern: "undefined symbol",                category: "Linker",             limitation: false },

    { pattern: "aligned.*assertion|mod .*31\\)",  category: "Memory Alignment",   limitation: false },
    { pattern: "incomplete object.*llvm size",    category: "Struct ABI",         limitation: false },
    { pattern: "disagree about type size",        category: "Struct ABI/size",    limitation: false },

    { pattern: "SIGSEGV|SIGBUS|nil\\?",           category: "Runtime Crash",      limitation: false },

    { pattern: "destroyed.*alloc.*dealloc|alloc.*dealloc.*destroyed",
                                                  category: "Memory/ARC",         limitation: false },
    { pattern: "AssertionDefect.*expected.*raised|expected.*error.*raised",
                                                  category: "Assertion/expect",   limitation: false },
    { pattern: "AssertionDefect",                 category: "Assertion",          limitation: false },

    { pattern: "allocCount.*deallocCount|deallocCount.*allocCount",
                                                  category: "Memory/count",       limitation: false },
    { pattern: "LEAK SUMMARY|definitely lost",    category: "Memory/leak",        limitation: false },
    { pattern: "uninitialised value|uninitialized value",
                                                  category: "Memory/uninit",      limitation: false },

    { pattern: "reOutputsDiffer",                 category: "Output mismatch",    limitation: false },
    { pattern: "reMsgsDiffer",                    category: "Compiler messages",  limitation: false },
    { pattern: "reExitcodesDiffer",               category: "Exit code",          limitation: false },

    { pattern: "Error:.*internal error",          category: "Compiler/internal",  limitation: false },
    { pattern: "Error:",                          category: "Compiler/error",     limitation: false }
  ];

# Classify a single entry: return {classification, limitation}
# Note: field is "classification" (not "category") to avoid shadowing
# any pre-existing .category field in the JSON.
def classify:
  (.given // "") + "\n" + (.expected // "") as $text |
  rules
  | map(.pattern as $p | select($text | test($p; "i")))
  | if length > 0
    then {classification: .[0].category, limitation: .[0].limitation}
    else {classification: "Unclassified", limitation: false}
    end;

# Merge a group of entries that share the same ".name".
# - String fields (given, expected, classification, result): joined with " | "
# - Non-string fields: taken from the first entry if they agree,
#   otherwise the first entry's value
def merge_group:
  . as $group |
  $group[0] as $base |
  ($base | keys | map(select(. != "name" and . != "machine" and . != "commit" and . != "branch"))) as $keys |
  ($keys | map(
    . as $k |
    {
      key: $k,
      value: (
        [$group[] | .[$k]] |
        if unique | length == 1
        then .[0]
        elif ($k == "classification")
        then $group[0][$k]        # keep classification from first entry
        elif all(type == "string")
        then join(" | ")
        elif all(type == "boolean")
        then ($group[0][$k])
        else ($group[0][$k] // "")
        end
      )
    }
  ) | from_entries) as $merged |
  $merged | $base + . | del(.machine, .commit, .branch);

# Main pipeline: classify each entry, then merge by .name
[ .[] | classify as $c | . + $c ]
| group_by(.name)
| map(merge_group)
| sort_by(.name)
