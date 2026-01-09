||| ic-wasm Profiling Parser
|||
||| Parses the output of `dfx canister call ... __get_profiling`
||| which returns execution traces from instrumented canisters.
|||
||| Format: vec { record { func_id: int32; cycles: int64 }; ... }
||| - Positive func_id = function entry
||| - Negative func_id = function exit
module DfxCoverage.IcWasm.ProfilingParser

import Data.List
import Data.List1
import Data.Maybe
import Data.String
import System
import System.File

import DfxCoverage.WasmTrace.TraceEntry

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| A single profiling entry from ic-wasm instrumentation
public export
record ProfilingEntry where
  constructor MkProfilingEntry
  funcId : Int        -- Positive = entry, Negative = exit
  cycles : Integer

public export
Show ProfilingEntry where
  show e = "func[" ++ show e.funcId ++ "] @ " ++ show e.cycles ++ " cycles"

||| Profiling result from __get_profiling call
public export
record ProfilingResult where
  constructor MkProfilingResult
  entries : List ProfilingEntry
  nextIdx : Maybe Int   -- For pagination if more data available

-- =============================================================================
-- Parsing Helpers (module level to avoid where scope issues)
-- =============================================================================

cleanNumber : String -> String
cleanNumber = pack . filter (\c => isDigit c || c == '-') . unpack

findSecondNumber : List String -> Maybe Integer
findSecondNumber [] = Nothing
findSecondNumber (w :: rest) =
  let numStr = cleanNumber w
  in case parseInteger numStr of
       Just n => Just n
       Nothing => findSecondNumber rest

findNumbers : List String -> Maybe (Int, Integer)
findNumbers [] = Nothing
findNumbers (w :: rest) =
  let numStr = cleanNumber w
  in case parseInteger numStr of
       Just n => case findSecondNumber rest of
                   Just m => Just (cast n, m)
                   Nothing => findNumbers rest
       Nothing => findNumbers rest

-- =============================================================================
-- Parsing
-- =============================================================================

||| Parse a single record like "record { 35 : int32; 49_846 : int64;}"
parseRecord : String -> Maybe ProfilingEntry
parseRecord s =
  let cleaned = trim s
      parts = words cleaned
  in case findNumbers parts of
       Just (fid, cyc) => Just $ MkProfilingEntry fid cyc
       Nothing => Nothing

parseRecordPart : String -> Maybe ProfilingEntry
parseRecordPart s =
  case break (== '}') (unpack s) of
    (content, _) => parseRecord (pack content)

||| Parse the full __get_profiling output
||| Expected format: (vec { record {...}; record {...}; ... }, null)
export
parseProfilingOutput : String -> ProfilingResult
parseProfilingOutput output =
  let parts = forget $ split (== '{') output
      records = mapMaybe parseRecordPart (drop 1 parts)
  in MkProfilingResult records Nothing

-- =============================================================================
-- Coverage Computation
-- =============================================================================

||| Get unique function IDs that were executed (entries only, not exits)
export
getExecutedFuncIds : ProfilingResult -> List Nat
getExecutedFuncIds pr =
  let entries = filter (\e => e.funcId > 0) pr.entries
      funcIds = map (\e => cast {to=Nat} e.funcId) entries
  in nub funcIds

||| Compute coverage percentage
||| @executed    List of executed function IDs
||| @totalFuncs  Total number of functions in the WASM
export
computeCoverage : List Nat -> Nat -> Double
computeCoverage executed totalFuncs =
  if totalFuncs == 0
    then 100.0
    else (cast (length (nub executed)) / cast totalFuncs) * 100.0

||| Coverage result
public export
record CoverageStats where
  constructor MkCoverageStats
  executedCount : Nat
  totalFunctions : Nat
  coveragePercent : Double
  executedFuncIds : List Nat

public export
Show CoverageStats where
  show cs = show cs.executedCount ++ "/" ++ show cs.totalFunctions ++
            " (" ++ show cs.coveragePercent ++ "%)"

-- =============================================================================
-- dfx Integration
-- =============================================================================

||| Call __get_profiling on a canister and parse the result
export
getProfilingData : String -> String -> IO (Either String ProfilingResult)
getProfilingData canisterId network = do
  let tmpFile = "/tmp/dfx_profiling_" ++ canisterId ++ ".txt"
  let cmd = "dfx canister call " ++ canisterId ++
            " '__get_profiling' '(0: int32)' --network " ++ network ++
            " > " ++ tmpFile ++ " 2>&1"
  exitCode <- system cmd
  if exitCode == 0
    then do
      Right content <- readFile tmpFile
        | Left err => pure $ Left $ "Failed to read output: " ++ show err
      _ <- system $ "rm -f " ++ tmpFile
      pure $ Right $ parseProfilingOutput content
    else pure $ Left "dfx call failed"

||| Get coverage stats for a canister
export
getCoverageStats : String -> String -> Nat -> IO (Either String CoverageStats)
getCoverageStats canisterId network totalFuncs = do
  Right pr <- getProfilingData canisterId network
    | Left err => pure $ Left err
  let executed = getExecutedFuncIds pr
  let percent = computeCoverage executed totalFuncs
  pure $ Right $ MkCoverageStats
    (length (nub executed))
    totalFuncs
    percent
    executed

-- =============================================================================
-- Convert to WasmTraceEntry
-- =============================================================================

||| Convert profiling entries to WasmTraceEntry format
export
toWasmTraces : ProfilingResult -> List WasmTraceEntry
toWasmTraces pr = mapMaybe toTrace pr.entries
  where
    toTrace : ProfilingEntry -> Maybe WasmTraceEntry
    toTrace e =
      if e.funcId > 0
        then Just $ MkWasmTraceEntry
               (cast e.funcId)   -- funcIdx : Nat
               0                 -- pc : Nat (not available from ic-wasm)
               "enter"           -- opcode : String
               0                 -- depth : Nat (not tracked in ic-wasm)
        else Nothing
