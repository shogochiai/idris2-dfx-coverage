# idris2-dfx-coverage

ICP Canister coverage analysis library for Idris2 CDK backend.

## Overview

This library provides tools for analyzing test coverage of ICP (Internet Computer Protocol) canisters. It integrates with the `lazy dfx ask` command to provide Step 4 coverage analysis.

## Architecture

```
idris2-dfx-coverage
├── CandidParser.idr       # Parse .did files to extract method signatures
├── CanisterCall.idr       # Execute dfx canister call and parse results
├── CoverageAnalyzer.idr   # Compare tested vs defined methods
└── Exclusions.idr         # Exclusion patterns for methods
```

## Modules

### CandidParser

Parses Candid interface files (`.did`) to extract:
- Method names
- Method signatures (args/returns)
- Method modes (query/update/oneway)

```idris
-- Parse a .did file
ci <- readCandidFile "can.did"
let methods = getMethodNames ci
```

### CanisterCall

Executes `dfx canister call` commands and captures results:

```idris
let opts = { defaultCallOptions | canisterId = "abc-123", network = "local" }
result <- callMethod opts "getVersion" ""
```

### CoverageAnalyzer

Analyzes coverage by comparing:
- Methods defined in Candid interface
- Methods actually tested via canister calls

```idris
let result = analyzeCoverage candidInterface callRecords exclusions
putStrLn $ "Coverage: " ++ show result.coveragePercent ++ "%"
```

### Exclusions

Defines patterns for excluding methods from coverage requirements:

```idris
let excl = prefixPattern "debug_" "Debug methods excluded"
let isExcl = isMethodExcluded [excl] "debug_print"  -- Just "Debug methods excluded"
```

## Usage with lazy dfx ask

This library is used by `LazyDfx` package's Step 4:

```bash
lazy dfx ask /path/to/project --steps=4
```

Step 4 will:
1. Parse the project's `.did` file
2. Start local IC network (if needed)
3. Deploy canister (if needed)
4. Execute canister calls for each method
5. Report coverage gaps

## Dependencies

- `base`
- `contrib`

## Building

```bash
pack build idris2-dfx-coverage.ipkg
```

## Related Projects

- `lazy/pkgs/LazyDfx` - The lazy dfx ask implementation
- `idris2-evm-coverage` - Similar coverage library for EVM/Yul
- `idris2-cdk` - Idris2 CDK for ICP canister development
