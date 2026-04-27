#!/usr/bin/env bash
# Build Shaders, copy to two locations, fshadeaot one, run a single Runner exe
# twice (once per shaders dir), diff outputs. The "AOT world" hash tests run inline.
set -e

THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$THIS_DIR/../../.." && pwd)"
BIN_DIR="$THIS_DIR/_test"
UNAOT_DIR="$BIN_DIR/unaot"
AOT_DIR="$BIN_DIR/aot"

rm -rf "$BIN_DIR" "$THIS_DIR/Shaders/bin" "$THIS_DIR/Shaders/obj" "$THIS_DIR/Runner/bin" "$THIS_DIR/Runner/obj"
mkdir -p "$UNAOT_DIR" "$AOT_DIR"

echo "== Building Shaders =="
dotnet build "$THIS_DIR/Shaders/Shaders.fsproj" -c Debug | tail -3
SHADERS_BIN_DIR="$THIS_DIR/Shaders/bin/Debug/net8.0"

cp "$SHADERS_BIN_DIR"/*.dll "$UNAOT_DIR/"
cp "$SHADERS_BIN_DIR"/*.dll "$AOT_DIR/"

echo ""
echo "== Running fshadeaot --new on AOT copy =="
"$REPO_ROOT/bin/Debug/net8.0/fshadeaot" "$AOT_DIR/FShade.Aot.Tests.Shaders.dll" --verbose 2>&1 | grep -E "patched|skip|error|FAILED|saving|^  " | head -20

echo ""
echo "== Building Runner =="
dotnet build "$THIS_DIR/Runner/Runner.fsproj" -c Debug | tail -3

UNAOT_OUT="$BIN_DIR/unaot.txt"
AOT_OUT="$BIN_DIR/aot.txt"

echo ""
echo "== Running Runner against UNAOT =="
dotnet "$THIS_DIR/Runner/bin/Debug/net8.0/FShade.Aot.Tests.Runner.dll" "$UNAOT_DIR" > "$UNAOT_OUT" || { echo "UNAOT runner failed"; cat "$UNAOT_OUT"; exit 1; }
echo "(unaot output written to $UNAOT_OUT, $(wc -l < "$UNAOT_OUT") lines)"

echo ""
echo "== Running Runner against AOT =="
dotnet "$THIS_DIR/Runner/bin/Debug/net8.0/FShade.Aot.Tests.Runner.dll" "$AOT_DIR" > "$AOT_OUT" || { echo "AOT runner failed"; cat "$AOT_OUT"; exit 1; }
echo "(aot output written to $AOT_OUT, $(wc -l < "$AOT_OUT") lines)"

echo ""
echo "== Diff GLSL between UNAOT and AOT (id lines stripped — those are expected to differ) =="
# Strip id lines because they DO differ (different hash function); GLSL must match.
diff <(grep -vE '^(id: |AotMarker)' "$UNAOT_OUT") <(grep -vE '^(id: |AotMarker)' "$AOT_OUT") && echo "GLSL DIFF EMPTY ✓" || { echo "GLSL DIFF NON-EMPTY ✗"; exit 1; }

echo ""
echo "== Verify AOT path was actually taken =="
AOT_DEFERRED=$(grep '^AotMarkerInvocations:' "$AOT_OUT" | awk '{print $2}')
AOT_PRECOMP=$(grep '^AotMarkerPrecomputedInvocations:' "$AOT_OUT" | awk '{print $2}')
UNAOT_DEFERRED=$(grep '^AotMarkerInvocations:' "$UNAOT_OUT" | awk '{print $2}')
UNAOT_PRECOMP=$(grep '^AotMarkerPrecomputedInvocations:' "$UNAOT_OUT" | awk '{print $2}')
echo "  unaot deferred / precomputed: $UNAOT_DEFERRED / $UNAOT_PRECOMP  (must both be 0)"
echo "  aot   deferred / precomputed: $AOT_DEFERRED / $AOT_PRECOMP  (both must be > 0)"
if [ "$UNAOT_DEFERRED" != "0" ] || [ "$UNAOT_PRECOMP" != "0" ]; then echo "UNEXPECTED unaot path took marker ✗"; exit 1; fi
if [ "$AOT_DEFERRED" = "0" ]; then echo "AOT deferred path did NOT fire ✗"; exit 1; fi
if [ "$AOT_PRECOMP" = "0" ]; then echo "AOT precomputed path did NOT fire ✗"; exit 1; fi

# Verify ids ARE different between unaot and aot (because hash schemes differ).
# (If they were the same, the AOT path would still be silently falling through to ofExpr.)
if diff -q <(grep '^id: ' "$UNAOT_OUT") <(grep '^id: ' "$AOT_OUT") >/dev/null; then
    echo "WARN: unaot and aot ids match exactly — verify marker is actually computing custom hash"
fi

echo ""
echo "== AOT-internal hash stability/uniqueness =="
# Re-run aot twice and compare ids
AOT_A=$(grep '^id: ' "$AOT_OUT" | head -1)
dotnet "$THIS_DIR/Runner/bin/Debug/net8.0/FShade.Aot.Tests.Runner.dll" "$AOT_DIR" > "$BIN_DIR/aot_again.txt"
AOT_B=$(grep '^id: ' "$BIN_DIR/aot_again.txt" | head -1)
if [ "$AOT_A" = "$AOT_B" ]; then
    echo "STABILITY ✓ (same args → same id across runs)"
else
    echo "STABILITY ✗ ($AOT_A vs $AOT_B)"; exit 1
fi
# uniqueness — first vs second case differ in args
ID1=$(grep -A1 '^=== frag_constantColor args=1 ===' "$AOT_OUT" | head -2 | tail -1)
ID2=$(grep -A1 '^=== frag_constantColor args=1 ===' "$AOT_OUT" | tail -2 | head -1 | head)
ID2_LINE=$(grep '^id: ' "$AOT_OUT" | sed -n '2p')
ID1_LINE=$(grep '^id: ' "$AOT_OUT" | sed -n '1p')
if [ "$ID1_LINE" != "$ID2_LINE" ]; then
    echo "UNIQUENESS ✓ (different args → different id)"
else
    echo "UNIQUENESS ✗"; exit 1
fi

echo ""
echo "ALL TESTS PASSED ✓"
