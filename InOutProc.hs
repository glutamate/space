module Main where

import Language.Haskell.Exts
--import Language.Haskell.Exts.Pretty
import Data.Maybe
import System.Environment

import Text.PrettyPrint
modDecls f (Module _ _ _ _ _ _ decls) = f decls

mapDecls f (Module  v1 v2 v3 v4 v5 v6 decls) = Module v1 v2 v3 v4 v5 (v6++[impUnsafe]) $ f decls

impUnsafe = ImportDecl {importLoc = SrcLoc {srcFilename = "TestSinks.hs", srcLine = 7, srcColumn = 1}, importModule = ModuleName "System.IO.Unsafe", importQualified = True, importSrc = False, importPkg = Nothing, importAs = Just (ModuleName "SysIOUnface"), importSpecs = Nothing}

maySink (SpliceDecl sloc (InfixApp (var)
 (QVarOp (UnQual (Symbol "*>"))) sink)) = Just (sloc, var,sink)
maySink _ = Nothing

maySrc (SpliceDecl sloc (InfixApp (Var (UnQual (Ident vnm)))
 (QVarOp (UnQual (Symbol "<*"))) src)) = Just (sloc, vnm, src)
maySrc _ = Nothing

isSrcOrSink d = isJust (maySrc d) || isJust (maySink d)
isSink d = isJust (maySink d)

transSrc d = 
    case maySrc d of 
      Just (sloc, vnm, src) ->  PatBind (sloc) (PVar (Ident vnm)) Nothing (UnGuardedRhs (App (Var (Qual (ModuleName "SysIOUnface") (Ident "unsafePerformIO"))) src)) (BDecls [])
      Nothing -> d

addMain decls = 
    let sinks = catMaybes $ map maySink decls
        iosink (sloc, exp, sink) = Qualifier $ App sink exp
--Qualifier (App (App (Var (UnQual (Ident "writeSink"))) exp) (sink))
        themain = PatBind (SrcLoc "foo" 1 1) (PVar (Ident "main")) Nothing (UnGuardedRhs (Do (map iosink sinks))) (BDecls [])
    in decls++[themain]

theTrans decls = filter (not . isSink) . addMain . map transSrc $ decls

main = do
  
  original:src:out:_ <- getArgs
  mmod <- fromParseResult `fmap` parseFile src
  --(print) $  mmod
  writeFile out $ prettyPrint $ mapDecls theTrans mmod



 