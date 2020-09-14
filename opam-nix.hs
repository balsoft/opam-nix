-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}

import Text.Parsec
import Data.Functor.Identity (Identity ())
import System.IO
import Data.Maybe (isNothing, maybeToList)
import Control.Monad (void)
import Data.List (intersperse, nub, isSuffixOf, isPrefixOf)

data OPAM
  = OPAM
  { name :: Maybe String
  , version :: Maybe String
  , nativeBuildInputs :: [String]
  , buildInputs :: [String]
  , buildPhase :: Maybe [[String]]
  , checkInputs :: [String]
  , checkPhase :: Maybe [[String]]
  , installPhase :: Maybe [[String]]
  , source :: Maybe String
  } deriving Show

-- Turn a description into a nix file
opam2nix :: OPAM -> String
opam2nix OPAM {..} =
  let
    normalize = nub . map (\case 'b':'a':'s':'e':'-':_ -> "base"; s -> s)
    buildInputs' = [ "findlib" ]
      -- conf-* packages are added with {build}, hack it so that it builds!
      ++ (filter (isPrefixOf "conf-") $ nativeBuildInputs)
      ++ buildInputs;
    nativeBuildInputs' = [ "dune", "opaline", "ocaml", "findlib" ]
      ++ (if any (isPrefixOf "conf-")
           (buildInputs' ++ checkInputs ++ nativeBuildInputs)
           then ["conf-pkg-config"]
           else [])
      ++ nativeBuildInputs
    installPhase' = case installPhase of
      Just c -> "mkdir -p $OCAMLFIND_DESTDIR\n" <> preparephase c
      Nothing -> "opaline -prefix $out -libdir $OCAMLFIND_DESTDIR"
    inputs = buildInputs' ++ checkInputs ++ nativeBuildInputs'
    deps = mconcat $ intersperse ", " $ normalize $ inputs
    sepspace = mconcat . intersperse " " . normalize
    quote s = "\""<>s<>"\""
    preparephase = mconcat . intersperse " "  . mconcat . intersperse ["\n"] . (fmap . fmap) quote
  in
    "{ stdenv, fetchzip, lib, " <>deps<> ", extraArgs ? { } }@args:\n"
  <>"stdenv.mkDerivation (let self = with self; with extraArgs; {\n"
  <>foldMap (\name' -> "  pname = \""<>name'<>"\";\n") name
  <>foldMap (\version' -> "  version = \""<>version'<>"\";\n") version
  <>foldMap (\url -> "  src = builtins.fetchTarball { url = \""<>url<>"\"; };\n") source
  <>"  outputs = [ \"out\" \"bin\" \"lib\" \"share\" ];\n"
  <>"  buildInputs = [ "<>sepspace buildInputs'<>" ];\n"
  <>"  checkInputs = [ "<>sepspace checkInputs<>" ];\n"
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
  <>"  installPhase = ''\nrunHook preInstall\n"
  <>installPhase'<>"\n"
  <>"if [[ -d $OCAMLFIND_DESTDIR/${pname} ]]; then mv $OCAMLFIND_DESTDIR/${pname} $lib; ln -s $lib $OCAMLFIND_DESTDIR/${pname}; else touch $lib; fi\n"
  <>"if [[ -d $out/bin ]]; then mv $out/bin $bin; ln -s $bin $out/bin; else touch $bin; fi\n"
  <>"if [[ -d $out/share ]]; then mv $out/share $share; ln -s $share $out/share; else touch $share; fi\n"
  <>"runHook postInstall\n  '';\n"
  <>"  preFixup = \"if [[ -d $bin ]]; then strip -S $bin/*; fi\";\n"
  <>"}; in self // extraArgs)\n"

update :: Maybe a -> a -> Maybe a
update old new = if isNothing old then Just new else old

-- Evaluate a Field and update OPAM description accordingly
evaluateField :: OPAM -> Field -> OPAM
evaluateField o@OPAM {..} = \case
  Name s -> o { name = update name s }
  Version s -> o { version = update version s }
  Depends s -> updateDeps s o
  OptionallyDepends s -> updateDeps s o
  Build e -> o {
    buildPhase = update buildPhase
      $ fmap ((fmap evaluateExp) . command) $ filter (\(Command _ info) -> not $ "with-test" `elem` info) e,
    checkPhase = update checkPhase
      $ fmap ((fmap evaluateExp) . command) $ filter (\(Command _ info) -> "with-test" `elem` info) e
  }
  Install e -> o {
    installPhase = update installPhase $ fmap ((fmap evaluateExp) . command) e
  }
  URL url -> o { source = update source url}
  Other _ -> o
  where
    updateDeps :: [Package] -> OPAM -> OPAM
    updateDeps packages o@OPAM {..} = o
      { buildInputs = buildInputs <>
        (fmap identifier $ filter (\(Package _ info) ->
                                    not $ ("with-test" `elem` info || "build" `elem` info)) packages)
      , nativeBuildInputs = nativeBuildInputs <>
        (fmap identifier $ filter (\(Package _ info) -> "build" `elem` info) packages)
      , checkInputs = checkInputs <>
        (fmap identifier $ filter (\(Package _ info) -> "with-test" `elem` info) packages)
      }

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
    repl :: ParsecT String u Identity [Either String Exp]
    repl = many ((Right <$> exp) <|> (Left <$> str)) <* eof
    str = many1 $ noneOf "%"
    exp = Var <$> between (string "%{") (string "}%") (many $ noneOf "}")
    replace (':':xs) = '.':replace xs
    replace (x:xs) = x:replace xs
    replace [] = []
    in
    \case
      Str s -> case parse repl "<string>" s of
        Left _ -> s
        Right eithers -> mconcat $ (\case
          Left s -> s
          Right e -> evaluateExp e) <$> eithers
      Var "name" -> "${pname}"
      Var "make" -> "make"
      Var "prefix" -> "$out"
      Var "pinned" -> "false"
      Var "jobs" -> "$NIX_BUILD_CORES"
      Var s
        | ":installed" `isSuffixOf` s -> "${if args ? "<>takeWhile (/=':') s<>" then \"true\" else \"false\"}"
        | otherwise -> "${"<>replace s<>"}"

data Command
  = Command
  { command :: [Exp]
  , additionalCommandInfo :: [String]
  } deriving Show

data Field
  = Name String
  | Version String
  | Depends [Package]
  | OptionallyDepends [Package]
  | Build [Command]
  | Install [Command]
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
    <|> OptionallyDepends <$> fieldParser "depopts" (listParser packageParser)
    <|> Build <$> fieldParser "build" (pure <$> try commandParser <|> listParser commandParser)
    <|> Install <$> fieldParser "install" (pure <$> try commandParser <|> listParser commandParser)
    <|> sectionParser "url" (URL <$> ((fieldParser "src" stringParser <|> fieldParser "archive" stringParser) <* many (noneOf "}")))
    <|> Other <$> (many (noneOf "\n") <* char '\n')

-- Field's structure is "name: value"
fieldParser :: String -> ParsecT String u Identity t -> ParsecT String u Identity t
fieldParser name valueParser = try
  $ between
  ((string name <* many (char ' ') <* char ':') >> many (oneOf " \n"))
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
stringParser = mconcat <$> between (char '"') (char '"') (many $ ((pure <$> noneOf "\\\"") <|> (string "\\" <> (pure <$> anyChar))))

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
      (OPAM Nothing Nothing [] [] Nothing [] Nothing Nothing Nothing) fs
