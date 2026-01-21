# idris2-dfx-coverage

ICP Canister (Idris2) 向けカバレッジ分析ツール。

## Coverage Calculation Formula

```
カバレッジ = 実行された関数 / テスト対象関数
           = Profiling traces  / Source Map names (または Dumpcases)
```

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  分子: 実行された関数                                        │
├─────────────────────────────────────────────────────────────┤
│  ic-wasm __get_profiling → WASM関数ID一覧                   │
│    ↓                                                        │
│  WASM関数ID → C関数名 → Idris関数名                         │
│    (Source Mapで変換)                                       │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│  分母: テスト対象関数                                        │
├─────────────────────────────────────────────────────────────┤
│  Option 1: idris2-c.map の names 配列                       │
│  Option 2: idris2 --dumpcases の出力                        │
└─────────────────────────────────────────────────────────────┘
```

## ic-wasm Profiling Usage

```bash
# 1. Instrument
ic-wasm xxx_stubbed.wasm -o xxx_instrumented.wasm instrument --stub-wasi

# 2. Deploy
dfx canister install xxx --wasm xxx_instrumented.wasm

# 3. Get traces (重要: __toggle_tracingは呼ばない！)
dfx canister call xxx __toggle_entry '()'  # ログ保持モード有効化
dfx canister call xxx someMethod '()'       # テスト実行
dfx canister call xxx '__get_profiling' '(0 : nat32)'
```

### Profiling Output Format

```
(vec {
  record { <func_id> : int32; <cycles> : int64 };  // 関数entry
  record { -<func_id> : int32; <cycles> : int64 }; // 関数exit (負のID)
}, <next_cursor>)
```

### Key Insight: Tracing State

```
canister_init終了時:
  is_init = 0  → トレース有効（デフォルト）

__toggle_tracing呼び出し:
  is_init XOR 1 → トレース無効化！

__toggle_entry呼び出し:
  is_entry = 1  → 呼び出し間でログ保持
```

**罠**: `__toggle_tracing`を呼ぶとトレースが**無効化**される。呼ばないこと。

## Source Map Integration

`build/idris2-c.map` から関数名を取得:

```json
{
  "names": [
    "Main.canisterUpdate",
    "Main.computeSum",
    "PrimIO.unsafePerformIO",
    ...
  ]
}
```

### Filtering Idris Functions

```bash
# Idris由来の関数のみ抽出
cat idris2-c.map | jq -r '.names[]' | grep -E "^(Main\.|Prelude\.|PrimIO\.)"
```

## Existing Parsers

- `src/DfxCoverage/IcWasm/ProfilingParser.idr` - `__get_profiling`出力パーサー
- `src/DfxCoverage/Idris2/DumpcasesParser.idr` - dumpcasesパーサー

## Limitations

1. **WASM関数ID↔Idris関数名の直接マッピングなし**
   - Emscriptenがfunction namesを保持しない
   - Source Mapの行番号マッピングから間接的に推測

2. **Cスタブ問題**
   - canister_entry.cがCスタブを提供する場合、Idris関数が呼ばれない
   - 解決: Cから`ensure_idris2_init()`を再実行してIdris main経由で呼び出す

3. **Dead Code Elimination**
   - `main`から到達不能な関数はWASMに含まれない
   - 解決: mainから全テスト対象関数を呼び出すようにする
