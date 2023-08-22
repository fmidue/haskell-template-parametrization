seed = return "Sommersemester 2023"

plain_makeYamlList {
makeYamlList :: String -> [String] -> IO String
makeYamlList header items = return $ header ++ ":\n" ++ init (unlines (map ("- " ++) items))
}

commonConfigGhcErrors {
#{plain_makeYamlList}
commonConfigGhcErrors = makeYamlList "configGhcErrors"
  [ "deprecation"
  , "empty-enumerations"
  , "identities"
  , "overflowed-literals"
  , "overlapping-patterns"
  , "tabs"
  ]
}

commonConfigHlintErrors {
#{plain_makeYamlList}
commonConfigHlintErrors = makeYamlList "configHlintErrors"
  [ "Avoid reverse"
  , "Collapse lambdas"
  , "Evaluate"
  , "Length always non-negative"
  , "Move brackets to avoid $"
  , "Redundant $"
  , "Redundant flip"
  , "Redundant fromInteger"
  , "Redundant fromIntegral"
  , "Redundant guard"
  , "Redundant id"
  , "Redundant lambda"
  , "Redundant list comprehension"
  , "Redundant maybe"
  , "Redundant multi-way if"
  , "Redundant negate"
  , "Redundant not"
  , "Redundant pair"
  , "Redundant section"
  , "Use !!"
  , "Use /="
  , "Use <"
  , "Use <="
  , "Use =="
  , "Use >"
  , "Use >="
  , "Use String"
  , "Use drop"
  , "Use elem"
  , "Use fst"
  , "Use head"
  , "Use id"
  , "Use init"
  , "Use last"
  , "Use left fold instead of right fold"
  , "Use list literal pattern"
  , "Use otherwise"
  , "Use product"
  , "Use right fold instead of left fold"
  , "Use snd"
  , "Use sum"
  , "Use take"
  , "Used otherwise as a pattern"
  , "Using all on tuple"
  , "Using and on tuple"
  , "Using any on tuple"
  , "Using concat on tuple"
  , "Using elem on tuple"
  , "Using foldr on tuple"
  , "Using length on tuple"
  , "Using maximum on tuple"
  , "Using minimum on tuple"
  , "Using null on tuple"
  , "Using or on tuple"
  , "Using product on tuple"
  , "Using sum on tuple"
  ]
}

commonConfigHlintGroups {
#{plain_makeYamlList}
commonConfigHlintGroups = makeYamlList "configHlintGroups"
  [ "monomorphic"
  , "teaching"
  ]
}

commonConfigHlintRules {
#{plain_makeYamlList}
commonConfigHlintRules = makeYamlList "configHlintRules"
  [ "'hint: {lhs: drop 1, rhs: tail, note: \"Be careful about empty lists, though\"}'"
  , "'warn: {lhs: last (take n x), rhs: x !! (n - 1), note: Check carefully that there is no possibility for index-too-large error}'"
  , "'warn: {lhs: foldr f c (reverse x), rhs: foldl'' (flip f) c x, note: \"reduces laziness\", name: Replace a fold by a strict fold}'"
  ]
}

commonConfigHlintSuggestions {
#{plain_makeYamlList}
commonConfigHlintSuggestions = makeYamlList "configHlintSuggestions"
  [ "Avoid lambda using `infix`"
  , "Move guards forward"
  , "Move map inside list comprehension"
  , "Reduce duplication"
  , "Redundant take"
  , "Replace a fold by a strict fold"
  , "Too strict if"
  , "Too strict maybe"
  , "Use section"
  ]
}

commonConfigLanguageExtensions {
#{plain_makeYamlList}
commonConfigLanguageExtensions = makeYamlList "configLanguageExtensions"
  [ "NoTemplateHaskell"
  , "TupleSections"
  ]
}
