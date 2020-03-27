{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}

import Text.Parsec
import Data.Functor.Identity (Identity ())
import System.Environment (getEnv)
import System.IO
import Data.Maybe (fromJust, isNothing, maybeToList)
import Control.Monad (void)
import Data.List (intersperse, nub, isPrefixOf)
import qualified Control.Applicative as A (optional)

data OPAM
  = OPAM
  { name :: Maybe String
  , version :: Maybe String
  , nativeBuildInputs :: Maybe [String]
  , buildInputs :: Maybe [String]
  , buildPhase :: Maybe [[String]]
  , checkInputs :: Maybe [String]
  , checkPhase :: Maybe [[String]]
  , source :: Maybe String
  } deriving Show

-- Turn a description into a nix file
opam2nix :: OPAM -> String
opam2nix OPAM {..} =
  let
    normalize = nub . map (\case 'b':'a':'s':'e':'-':_ -> "base"; s -> s)
    buildInputs' = [ "findlib" ] ++ mconcat (maybeToList buildInputs);
    checkInputs' = mconcat $ maybeToList checkInputs
    nativeBuildInputs' = [ "dune", "opaline", "ocaml", "findlib" ]
      ++ (if any (isPrefixOf "conf-")
           (buildInputs' ++ checkInputs' ++ mconcat (maybeToList nativeBuildInputs))
           then ["conf-pkg-config"]
           else [])
      ++ mconcat (maybeToList nativeBuildInputs)
    inputs = buildInputs' ++ checkInputs' ++ nativeBuildInputs'
    deps = mconcat $ intersperse ", " $ normalize $ inputs
    sepspace = mconcat . intersperse " " . normalize
    preparephase = mconcat . intersperse " " . mconcat . intersperse ["\n"]
  in
    "{ stdenv, fetchzip, " <>deps<> ", extraArgs ? { } }:\n"
  <>"\n" -- Awful hack to allow this to evaluate even if some of the variables are undefined
  <>"stdenv.mkDerivation (let self = with self; with extraArgs; {\n"
  <>foldMap (\name' -> "  pname = \""<>name'<>"\";\n") name
  <>foldMap (\version' -> "  version = \""<>version'<>"\";\n") version
  <>foldMap (\url -> "  src = builtins.fetchTarball { url = \""<>url<>"\"; };\n") source
  <>"  buildInputs = [ "<>sepspace buildInputs'<>" ];\n"
  <>"  checkInputs = [ "<>sepspace checkInputs'<>" ];\n"
  <>"  nativeBuildInputs = [ "<>sepspace nativeBuildInputs'<>" ];\n"
  <>"  propagatedBuildInputs = buildInputs;\n"
  <>"  propagatedNativeBuildInputs = nativeBuildInputs;\n"
  <>foldMap (\buildPhase' ->
                "  buildPhase = ''runHook preBuild\n"
              <> preparephase buildPhase'
              <>"\nrunHook postBuild\n'';\n") buildPhase
  <>foldMap (\checkPhase' ->
                "  checkPhase = ''runHook preCheck\n"
              <>preparephase checkPhase'
              <>"\nrunHook postCheck\n'';\n") checkPhase
  <>"  installPhase = ''\nrunHook preInstall\nopaline -prefix "
  <>"$out -libdir $OCAMLFIND_DESTDIR\nrunHook postInstall\n'';\n"
  <>"}; in self // extraArgs)\n"

update :: Maybe a -> a -> Maybe a
update old new = if isNothing old then Just new else old

-- Evaluate a Field and update OPAM description accordingly
evaluateField :: OPAM -> Field -> OPAM
evaluateField o@OPAM {..} = \case
  Name s -> o { name = update name s }
  Version s -> o { version = update version s }
  Depends s -> o {
    buildInputs = update buildInputs $
      fmap identifier $ filter (\(Package _ info) ->
                                  not $ ("with-test" `elem` info || "build" `elem` info)) s,
    nativeBuildInputs = update nativeBuildInputs $
      fmap identifier $ filter (\(Package _ info) -> "build" `elem` info) s,
    checkInputs = update checkInputs $
      fmap identifier $ filter (\(Package _ info) -> "with-test" `elem` info) s
  }
  Build e -> o {
    buildPhase = update buildPhase
      $ fmap ((fmap evaluateExp) . command) $ filter (\(Command _ info) -> not $ "with-test" `elem` info) e,
    checkPhase = update checkPhase
      $ fmap ((fmap evaluateExp) . command) $ filter (\(Command _ info) -> "with-test" `elem` info) e
  }
  URL url -> o { source = update source url}
  Other _ -> o

evaluateFields :: OPAM -> [Field] -> OPAM
evaluateFields = foldl evaluateField


-- Descriptions for various Fields of an opam file

data Package
  = Package
  { identifier :: String
  , additionalPackageInfo :: [String]
  } deriving Show

-- An expression as found in a Command
data Exp = Str String | Var String deriving Show

evaluateExp :: Exp -> String
evaluateExp =
  let
    repl ('%':'{':xs) = '$':'{':repl xs
    repl ('}':'%':xs) = '}':repl xs
    repl (':':_:_:_:'}':'%':xs) = '}':repl xs
    repl (x:xs) = x:repl xs
    repl "" = ""
    in
    \case
      Str s -> repl s
      Var "name" -> "${pname}"
      Var "make" -> "make"
      Var "prefix" -> "$out"
      Var "jobs" -> "1"
      Var s -> "${"<>s<>"}"

data Command
  = Command
  { command :: [Exp]
  , additionalCommandInfo :: [String]
  } deriving Show

data Field
  = Name String
  | Version String
  | Depends [Package]
  | Build [Command]
  | URL String
  | Other String
  deriving Show


-- An opam file is a collection of fields,
opamFile :: ParsecT String u Identity [Field]
opamFile = many field <* eof

-- Each has a name and a type;
field :: ParsecT String u Identity Field
field = Name <$> fieldParser "name" stringParser
    <|> Version <$> fieldParser "version" stringParser
    <|> Depends <$> fieldParser "depends" (listParser packageParser)
    <|> Build <$> fieldParser "build" (pure <$> try commandParser <|> listParser commandParser)
    <|> sectionParser "url" (URL <$> (fieldParser "src" stringParser <* many (noneOf "}")))
    <|> Other <$> (many (noneOf "\n") <* char '\n')

-- Field's structure is "name: value"
fieldParser :: String -> ParsecT String u Identity t -> ParsecT String u Identity t
fieldParser name valueParser = try
  $ between
  (string (name<>":") >> many (oneOf " \n"))
  (many $ oneOf " \n")
  valueParser <* commentParser

-- Sections's structure is "name { fields }"
sectionParser :: String -> ParsecT String u Identity t -> ParsecT String u Identity t
sectionParser name valueParser = try
  $ between
  (string name >> many (oneOf " ") >> string "{" >> many (oneOf " \n"))
  (many (oneOf " \n") >> char '}' >> char '\n')
  valueParser

-- String is enclosed in quotes
stringParser :: ParsecT String u Identity String
stringParser = between (char '"') (char '"') (many $ noneOf "\"")

-- Expression is either a string or a variable
expParser :: ParsecT String u Identity Exp
expParser = try (Str <$> stringParser)
        <|> Var <$> many1 (noneOf " \n\"{}[]")

-- "Additional Info" is additional information about a package or command, "{like-this}"
additionalInfoParser :: ParsecT String u Identity [String]
additionalInfoParser = option [] $ try
  $ between (many (char ' ') >> char '{') (char '}')
  ((many $ noneOf " &}") `sepBy` (oneOf " &"))

-- Command is a [expressions] with additionional information
commandParser :: ParsecT String u Identity Command
commandParser = Command <$> (listParser $ try expParser) <*> additionalInfoParser

-- Comment starts with # and goes to the end of line
commentParser :: ParsecT String u Identity ()
commentParser = optional $ do
  void $ string "#"
  many $ noneOf "\n"

-- Package is a "string" with additional information
packageParser :: ParsecT String u Identity Package
packageParser = Package <$> stringParser <*> additionalInfoParser


listParser :: ParsecT String u Identity t -> ParsecT String u Identity [t]
listParser valueParser =
  between (char '[') (char ']') $ between startPadding endPadding
    valueParser `sepBy` sep
  where
    startPadding = sep
    endPadding = whiteSpace
    sep = (whiteSpace >> commentParser) <|> whiteSpace
    whiteSpace = optional $ many $ oneOf " \n"

main :: IO ()
main = do
  hSetEncoding stdin utf8
  getContents >>= \s -> case parse opamFile "(unknown)" s of
    Left e -> print e
    Right fs -> putStrLn $ opam2nix $ evaluateFields
      (OPAM Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) fs
