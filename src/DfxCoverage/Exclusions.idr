||| Exclusion Patterns
|||
||| Define and apply exclusion patterns for canister methods.
||| Allows excluding certain methods from coverage requirements.
module DfxCoverage.Exclusions

import Data.List
import Data.Maybe
import Data.String
import System.File

%default covering

-- =============================================================================
-- Types
-- =============================================================================

||| Exclusion pattern type
public export
data PatternType
  = Exact         -- Exact method name match
  | Prefix        -- Method name starts with pattern
  | Suffix        -- Method name ends with pattern
  | Contains      -- Method name contains pattern
  | Regex         -- Full regex (not implemented yet)

public export
Show PatternType where
  show Exact = "exact"
  show Prefix = "prefix"
  show Suffix = "suffix"
  show Contains = "contains"
  show Regex = "regex"

||| Single exclusion pattern
public export
record ExclPattern where
  constructor MkExclPattern
  pattern : String
  patternType : PatternType
  reason : String

public export
Show ExclPattern where
  show p = show p.patternType ++ ":" ++ p.pattern ++ " (" ++ p.reason ++ ")"

-- =============================================================================
-- Pattern Matching
-- =============================================================================

||| Check if a method name matches an exclusion pattern
matchesPattern : ExclPattern -> String -> Bool
matchesPattern excl methodName =
  case excl.patternType of
    Exact => methodName == excl.pattern
    Prefix => isPrefixOf excl.pattern methodName
    Suffix => isSuffixOf excl.pattern methodName
    Contains => isInfixOf excl.pattern methodName
    Regex => False  -- Not implemented

||| Check if a method should be excluded
|||
||| @patterns List of exclusion patterns
||| @methodName Method name to check
||| @returns Just reason if excluded, Nothing otherwise
public export
isMethodExcluded : List ExclPattern -> String -> Maybe String
isMethodExcluded patterns methodName =
  map (.reason) $ find (\p => matchesPattern p methodName) patterns

-- =============================================================================
-- Pattern Creation Helpers
-- =============================================================================

||| Create exact match pattern
public export
exactPattern : String -> String -> ExclPattern
exactPattern name reason = MkExclPattern name Exact reason

||| Create prefix pattern
public export
prefixPattern : String -> String -> ExclPattern
prefixPattern pre reason = MkExclPattern pre Prefix reason

||| Create suffix pattern
public export
suffixPattern : String -> String -> ExclPattern
suffixPattern suf reason = MkExclPattern suf Suffix reason

||| Create contains pattern
public export
containsPattern : String -> String -> ExclPattern
containsPattern sub reason = MkExclPattern sub Contains reason

-- =============================================================================
-- Default Exclusions
-- =============================================================================

||| Default exclusions for ICP canisters
||| These are common methods that may not need direct testing
public export
defaultExclusions : List ExclPattern
defaultExclusions =
  [ -- Internal/lifecycle methods
    prefixPattern "__" "Internal method"
  , exactPattern "canister_init" "Lifecycle method"
  , exactPattern "canister_pre_upgrade" "Lifecycle method"
  , exactPattern "canister_post_upgrade" "Lifecycle method"
  , exactPattern "canister_inspect_message" "Lifecycle method"
  , exactPattern "canister_heartbeat" "Lifecycle method"
  , exactPattern "canister_global_timer" "Lifecycle method"

    -- Debug/development methods
  , prefixPattern "debug_" "Debug method"
  , prefixPattern "test_" "Test helper method"

    -- Version/metadata (often trivial)
  , exactPattern "getVersion" "Metadata method"
  , exactPattern "get_version" "Metadata method"
  ]

-- =============================================================================
-- File Loading
-- =============================================================================

||| Parse a single exclusion line
||| Format: TYPE:PATTERN # reason
||| Example: prefix:debug_ # Debug methods excluded
parseExclusionLine : String -> Maybe ExclPattern
parseExclusionLine line =
  let trimmed = trim line
  in if null trimmed || isPrefixOf "#" trimmed
       then Nothing
       else parsePattern trimmed
  where
    parsePattern : String -> Maybe ExclPattern
    parsePattern s =
      case break (== '#') (unpack s) of
        (patternPart, reasonPart) =>
          let pattern = trim (pack patternPart)
              reason = trim (pack (drop 1 reasonPart))
          in case break (== ':') (unpack pattern) of
               (typePart, namePart) =>
                 let ptype = case toLower (trim (pack typePart)) of
                               "exact" => Just Exact
                               "prefix" => Just Prefix
                               "suffix" => Just Suffix
                               "contains" => Just Contains
                               _ => Nothing
                     name = trim (pack (drop 1 namePart))
                 in case ptype of
                      Just t => Just $ MkExclPattern name t
                                          (if null reason then "Excluded" else reason)
                      Nothing => Nothing

||| Load exclusion patterns from a file
|||
||| @path Path to exclusion file
public export
loadExclusionFile : String -> IO (List ExclPattern)
loadExclusionFile path = do
  Right content <- readFile path
    | Left _ => pure []
  let ls = lines content
  pure $ mapMaybe parseExclusionLine ls

||| Load exclusion patterns from multiple files
public export
loadExclusionFiles : List String -> IO (List ExclPattern)
loadExclusionFiles paths = do
  patternLists <- traverse loadExclusionFile paths
  pure $ concat patternLists

||| Combine default exclusions with loaded patterns
public export
loadWithDefaults : List String -> IO (List ExclPattern)
loadWithDefaults paths = do
  loaded <- loadExclusionFiles paths
  pure $ defaultExclusions ++ loaded
