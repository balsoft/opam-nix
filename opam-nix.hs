-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE FlexibleContexts, LambdaCase, RecordWildCards #-}

import Control.Monad (void)
import Data.Bool (bool)
import Data.Functor.Identity (Identity ())
import Data.List (stripPrefix, intercalate, intersperse, nub, isSuffixOf, isPrefixOf)
import Data.Maybe (catMaybes, isNothing, maybeToList)
import Data.Set (Set, difference, fromList, toList)
import qualified Data.Set as Set (map)
import System.IO
import Text.Parsec

data OPAM
  = OPAM
  { name :: Maybe String
  , version :: Maybe String
  , nativeBuildInputs :: Maybe [String]
  , buildInputs :: Maybe [String]
  , buildPhase :: Maybe [[String]]
  , checkInputs :: Maybe [String]
  , checkPhase :: Maybe [[String]]
  , installPhase :: Maybe [[String]]
  , source :: Maybe (String, [Hash])
  } deriving Show

data PackageInputs = PackageInputs
  { piNativeBuildInputs :: Set String
  , piBuildInputs :: Set String
  , piCheckInputs :: Set String
  }

printInputs :: PackageInputs -> String
printInputs PackageInputs{..} =
  "{ stdenv, fetchurl, lib"
  <> pPrintInputs piNativeBuildInputs
  <> pPrintInputs piBuildInputs
  <> pPrintInputs (Set.map (<>" ? null") onlyCheckInputs)
  <> ", extraArgs ? { } }@args:\n"
  where
    -- to avoide duplicates
    onlyCheckInputs = (piCheckInputs `difference` piBuildInputs) `difference` piNativeBuildInputs
    sepcoma :: [String] -> String
    sepcoma = intercalate ", "
    pPrintInputs :: Set String -> String
    pPrintInputs inputs =
      bool "" (", " <> sepcoma (map normalizeInput $ toList inputs)) $ not $ null inputs
    normalizeInput input
      | "base-" `isPrefixOf` input = "base"
      | otherwise = input

-- Turn a description into a nix file
opam2nix :: OPAM -> String
opam2nix OPAM {..} =
  let
    normalize = nub . map (\case 'b':'a':'s':'e':'-':_ -> "base"; s -> s)
    buildInputs' = [ "findlib" ]
      -- conf-* packages are added with {build}, hack it so that it builds!
      ++ (filter (isPrefixOf "conf-") $ mconcat $ maybeToList nativeBuildInputs)
      ++ mconcat (maybeToList buildInputs);
    checkInputs' = mconcat $ maybeToList checkInputs
    nativeBuildInputs' = [ "dune_2", "opaline", "ocaml", "findlib", "gnutar" ]
      ++ (if any (isPrefixOf "conf-")
           (buildInputs' ++ checkInputs' ++ mconcat (maybeToList nativeBuildInputs))
           then ["conf-pkg-config"]
           else [])
      ++ mconcat (maybeToList nativeBuildInputs)
    installPhase' = case installPhase of
      Just c -> "mkdir -p $OCAMLFIND_DESTDIR\n" <> preparephase c
      Nothing -> "opaline -prefix $out -libdir $OCAMLFIND_DESTDIR"
    sepspace = mconcat . intersperse " " . normalize
    quote s = "\""<>s<>"\""
    preparephase = mconcat . intersperse " "  . mconcat . intersperse ["\n"] . (fmap . fmap) quote
    packageInputs = PackageInputs
      { piNativeBuildInputs = fromList nativeBuildInputs'
      , piBuildInputs = fromList buildInputs'
      , piCheckInputs = fromList checkInputs'
      }
  in
    printInputs packageInputs
  <>"stdenv.mkDerivation (let self = with self; with extraArgs; {\n"
  <>foldMap (\name' -> "  pname = \""<>name'<>"\";\n") name
  <>foldMap (\version' -> "  version = \""<>version'<>"\";\n") version
  <>foldMap (\(url, hashes) -> "  src = fetchurl { url = \""<>url<>"\"; " <> handleHashes hashes <> " };\n") source
  <>"  outputs = [ \"out\" \"bin\" \"lib\" \"share\" ];\n"
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
  <>"  installPhase = ''\nrunHook preInstall\n"
  <>installPhase'<>"\n"
  <>"if [[ -d $OCAMLFIND_DESTDIR/${pname} ]]; then mv $OCAMLFIND_DESTDIR/${pname} $lib; ln -s $lib $OCAMLFIND_DESTDIR/${pname}; else touch $lib; fi\n"
  <>"if [[ -d $out/bin ]]; then mv $out/bin $bin; ln -s $bin $out/bin; else touch $bin; fi\n"
  <>"if [[ -d $out/share ]]; then mv $out/share $share; ln -s $share $out/share; else touch $share; fi\n"
  <>"runHook postInstall\n  '';\n"
  <>"  preFixup = \"if [[ -d $bin ]]; then strip -S $bin/*; fi\";\n"
  <>"}; in self // extraArgs)\n"
  where
    handleHashes :: [Hash] -> String
    handleHashes = concatMap $ \hash ->
      getPrefix hash <> " = \"" <> getBytes hash <> "\";\n"

    getPrefix :: Hash -> String
    getPrefix (Sha256Hash _) = "sha256"
    getPrefix (Sha512Hash _) = "sha512"

    getBytes :: Hash -> String
    getBytes (Sha256Hash h) = h
    getBytes (Sha512Hash h) = h

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
  Install e -> o {
    installPhase = update installPhase $ fmap ((fmap evaluateExp) . command) e
  }
  Sources url hashes -> o { source = update source (url, hashes)}
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
  | Build [Command]
  | Install [Command]
  | Sources String [Hash]
  | Other String
  deriving Show

data Hash
  = Sha256Hash String
  | Sha512Hash String
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
    <|> Install <$> fieldParser "install" (pure <$> try commandParser <|> listParser commandParser)
    <|> sectionParser "url" (Sources <$>
                             (fieldParser "src" stringParser <|> fieldParser "archive" stringParser) <*>
                             (catMaybes <$> fieldParser "checksum" ((pure <$> try hashParser) <|> listParser hashParser)) <* many (noneOf "}")
                            )
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

hashParser :: ParsecT String u Identity (Maybe Hash)
hashParser = between (many $ char ' ') (many $ oneOf " \n") $ do
  parse <$> stringParser
  where
    parse :: String -> Maybe Hash
    parse s | Just hash <- stripPrefix "sha256=" s = Just $ Sha256Hash hash
            | Just hash <- stripPrefix "sha512=" s = Just $ Sha512Hash hash
    parse _ = Nothing


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
      (OPAM Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) fs
