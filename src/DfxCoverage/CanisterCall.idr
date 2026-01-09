||| Canister Call Execution
|||
||| Executes dfx canister call commands and parses results.
||| Used to test canister methods and track coverage.
module DfxCoverage.CanisterCall

import Data.List
import Data.Maybe
import Data.String
import System
import System.File
import System.Clock

import DfxCoverage.CandidParser

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Result of a canister call
public export
data CallResult
  = CallSuccess String      -- Successful call with result
  | CallError String        -- Call failed with error message
  | CallTimeout             -- Call timed out
  | CallNotFound            -- Method not found

public export
Show CallResult where
  show (CallSuccess s) = "Success: " ++ substr 0 50 s ++ if length s > 50 then "..." else ""
  show (CallError e) = "Error: " ++ e
  show CallTimeout = "Timeout"
  show CallNotFound = "NotFound"

||| Record of a single call attempt
public export
record CallRecord where
  constructor MkCallRecord
  methodName : String
  args : String
  result : CallResult
  durationMs : Nat

public export
Show CallRecord where
  show r = r.methodName ++ "(" ++ r.args ++ ") -> " ++ show r.result ++
           " (" ++ show r.durationMs ++ "ms)"

||| Options for canister calls
public export
record CallOptions where
  constructor MkCallOptions
  canisterId : String
  network : String          -- "local", "ic", etc.
  timeoutMs : Nat           -- Timeout in milliseconds
  dfxPath : String          -- Path to dfx binary (usually "dfx")

public export
defaultCallOptions : CallOptions
defaultCallOptions = MkCallOptions
  { canisterId = ""
  , network = "local"
  , timeoutMs = 30000       -- 30 seconds default
  , dfxPath = "dfx"
  }

-- =============================================================================
-- Command Execution
-- =============================================================================

||| Build dfx canister call command
buildCallCommand : CallOptions -> String -> String -> String
buildCallCommand opts method args =
  opts.dfxPath ++ " canister call " ++ opts.canisterId ++
  " " ++ method ++
  (if null args then "" else " '" ++ args ++ "'") ++
  " --network " ++ opts.network

||| Execute a shell command and capture output
||| Returns (exit code, stdout, stderr)
executeCommand : String -> IO (Int, String, String)
executeCommand cmd = do
  -- Use temporary files to capture output
  let stdoutFile = "/tmp/dfx_stdout_" ++ show !time ++ ".txt"
  let stderrFile = "/tmp/dfx_stderr_" ++ show !time ++ ".txt"
  let fullCmd = cmd ++ " > " ++ stdoutFile ++ " 2> " ++ stderrFile

  exitCode <- system fullCmd

  Right stdout <- readFile stdoutFile
    | Left _ => pure (exitCode, "", "")
  Right stderr <- readFile stderrFile
    | Left _ => pure (exitCode, stdout, "")

  -- Cleanup temp files
  _ <- system $ "rm -f " ++ stdoutFile ++ " " ++ stderrFile

  pure (exitCode, trim stdout, trim stderr)

||| Get current time in nanoseconds
getTimeNs : IO Integer
getTimeNs = do
  t <- clockTime Monotonic
  pure $ nanoseconds t

||| Parse call result from dfx output
parseCallResult : Int -> String -> String -> CallResult
parseCallResult exitCode stdout stderr =
  if exitCode == 0
    then CallSuccess stdout
    else if isInfixOf "method not found" (toLower stderr)
      then CallNotFound
      else CallError stderr

-- =============================================================================
-- Canister Call API
-- =============================================================================

||| Call a canister method with arguments
|||
||| @opts Call options (canisterId, network, etc.)
||| @method Method name to call
||| @args Arguments in Candid text format (e.g., "(42, \"hello\")")
public export
callMethod : CallOptions -> String -> String -> IO CallRecord
callMethod opts method args = do
  let cmd = buildCallCommand opts method args
  startTime <- getTimeNs
  (exitCode, stdout, stderr) <- executeCommand cmd
  endTime <- getTimeNs

  let result = parseCallResult exitCode stdout stderr
  let durationNs = endTime - startTime
  let durationMs = cast {to=Nat} (durationNs `div` 1000000)

  pure $ MkCallRecord method args result durationMs

||| Call a canister method without arguments (query)
public export
callQuery : CallOptions -> String -> IO CallRecord
callQuery opts method = callMethod opts method ""

||| Test multiple methods and return call records
public export
testMethods : CallOptions -> List String -> IO (List CallRecord)
testMethods opts methods = traverse (\m => callQuery opts m) methods

||| Test all methods from a Candid interface
public export
testInterface : CallOptions -> CandidInterface -> IO (List CallRecord)
testInterface opts ci = testMethods opts (getMethodNames ci)

-- =============================================================================
-- Result Analysis
-- =============================================================================

||| Check if a call was successful
public export
isSuccess : CallRecord -> Bool
isSuccess r = case r.result of
  CallSuccess _ => True
  _ => False

||| Get successful calls from a list
public export
getSuccessfulCalls : List CallRecord -> List CallRecord
getSuccessfulCalls = filter isSuccess

||| Get failed calls from a list
public export
getFailedCalls : List CallRecord -> List CallRecord
getFailedCalls = filter (not . isSuccess)

||| Calculate success rate
public export
successRate : List CallRecord -> Double
successRate [] = 0.0
successRate rs =
  let totalCount = cast {to=Double} (length rs)
      successCount = cast {to=Double} (length (getSuccessfulCalls rs))
  in (successCount / totalCount) * 100.0
