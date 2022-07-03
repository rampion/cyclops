*`cyclops`* is a command-line option parser for Haskell applications.

This alpha release is a proof-of-concept of the core idea, which is to
allow users to specify option properties (placeholder text, flags, description,
default value) as visually close as possible to the variable binding, via
`ViewPatterns`, `TypeApplications` and `DataKinds`:

```haskell
-- password_generator.hs
main :: IO ()
main = do
  ( switch @'["d", "debug"] 
           @"Show debugging statements"
    -> debug

   ,option @'["dictionary"] 
           @"PATH" 
           @"File to draw words from"
           .defaultTo @"/usr/share/dict/words"
    -> dictionaryPath

   ,option @'["s", "seed"]
           @"NUM"
           @"Seed value for PRNG" 
           .readable
    -> seed

   ,positional @"count"
               @"Number of passwords to generate"
               .readable
    -> count

   ,positional @"template"
               @"Template for generated passwords"
               .some
               .defaultTo @"w w w w"
    -> template

   ) <- getOps Conf
      { description = "Generate pseudorandom passwords using a template." 
      , version = "1.0.0.20220702"
      }

  when debug do
    putStrLn "debugging enabled"

  dict <- lines <$> readFile dictionaryPath

  case seed of
    Nothing -> mempty
    Just n  -> setStdGen n

  replicateM_ count do
    putStrLn . unwords =<< traverse generate template 
```

The intent is to make it as easy as possible for the developer to get
command-line arguments from users with automatically generated
parsers and documentation:

```
$ password_generator --help
USAGE: password_generator [-h|-?|--help] [-v|--version] [-d|--debug] [--dictionary=PATH] [-s|--seed=NUM] count [template ...]

Generate pseudorandom passwords using a template.

Arguments:
  [-h|-?|--help]        - Show this message and exit
  [-v|--version]        - Show the version (1.0.0.20220702) and exit
  [-d|--debug]          - Show debugging statements
  [--dictionary=PATH]   - File to draw words from (default: /usr/share/dict/words)
  [-s|--seed=NUM]       - Seed value for PRNG
  count                 - Number of passwords to generate
  [template ...]        - Template for gnerated passwords (default: w w w w)
```

The current implementation is a wrapper around `optparse-applicative`, though
this may change in the future to allow argument parsers to have side-effects in
IO (e.g. a parser for readable file paths).

Desired, but not yet implemented:

-   Grouping mutually-exclusive options:

    ```haskell
      ,group
        ( flagged @'["log-level"] @"[NONE|ERROR|WARN|INFO|DEBUG|TRACE]" @"Specify the logging level"
        , flag @'["n","none"]  @"Show no log messages"              NONE
        , flag @'["e","error"] @"Show error logs"                   ERROR
        , flag @'["w","warn"]  @"Show warnings and above"           WARN
        , flag @'["i","info"]  @"Show informational logs and above" INFO
        , flag @'["d","debug"] @"Show debugging logs and above"     DEBUG
        , flag @'["t","trace"] @"Show all log messages"             TRACE
        ) -> logLevel
    ```

-   Folding multiple option values

    ```haskell
      ,option @["input"]
              @"File to read input from"
              .many
       -> Last (ReadablePath input)
    ```
