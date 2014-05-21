{-# OPTIONS_GHC -Wall #-}

-- Module adapted from https://github.com/sseefried/js-good-parts

module AST (
  -- JSString, JSName
  JSString, JSName,
  unJSString, unJSName,
  jsString, jsName,

  -- * Data types
  JSDouble(..), JSFloat(..),
  JSVarStatement(..), JSVarDecl(..), JSStatement(..),
  JSExpressionStatement(..), JSLValue(..), JSRValue(..), JSExpression(..),
  JSInvocation(..), JSRefinement(..),
  JSLiteral(..), JSObjectLiteral(..), JSObjectField(..), JSArrayLiteral(..),
  JSFunctionLiteral(..), JSFunctionBody(..)
) where

import Text.PrettyPrint.Leijen
import PrettyPrec
import NonEmptyList

data JSName = JSName { unJSName :: String }

jsName :: String -> JSName
jsName = JSName

data JSString = JSString { unJSString :: String }

jsString :: String -> JSString
jsString = JSString

newtype JSDouble = JSDouble Double
newtype JSFloat = JSFloat Float

data JSVarStatement = JSVarStatement [JSVarDecl]
data JSVarDecl = JSVarDecl JSName (Maybe JSExpression)

data JSStatement
  = JSStatementExpression JSExpressionStatement

data JSExpressionStatement
  = JSESApply (NonEmptyList JSLValue) JSRValue

data JSLValue = JSLValue JSName [([JSInvocation], JSRefinement)]

data JSRValue
  = JSRVInvoke    (NonEmptyList JSInvocation)
  | JSRVRefinement

data JSExpression
  = JSExpressionLiteral    JSLiteral
  | JSExpressionName       JSName
  | JSExpressionInvocation JSExpression     JSInvocation
  | JSExpressionRefinement JSExpression     JSRefinement
  | JSExpressionNew        JSExpression     JSInvocation

data JSInvocation = JSInvocation [JSExpression]

data JSRefinement
  = JSProperty JSName
  | JSSubscript JSExpression

data JSLiteral
  = JSLiteralDouble   JSDouble
  | JSLiteralFloat    JSFloat
  | JSLiteralString   JSString
  | JSLiteralObject   JSObjectLiteral
  | JSLiteralArray    JSArrayLiteral
  | JSLiteralFunction JSFunctionLiteral

data JSObjectLiteral = JSObjectLiteral [JSObjectField]
data JSObjectField  = JSObjectField (Either JSName JSString) JSExpression
data JSArrayLiteral = JSArrayLiteral [JSExpression]
data JSFunctionLiteral = JSFunctionLiteral (Maybe JSName) [JSName] JSFunctionBody
data JSFunctionBody = JSFunctionBody [JSVarStatement] [JSStatement]

instance Pretty JSString where
  pretty s = char '\'' <> text (unJSString s) <> char '\''

instance Pretty JSName where
  pretty = text . unJSName

sepWith :: Pretty a => Doc -> [a] -> Doc
sepWith s = encloseSep empty empty s . map pretty

sepWith' :: Pretty a => Doc -> NonEmptyList a -> Doc
sepWith' s = encloseSep empty empty s . map pretty . toList

params :: Pretty a => [a] -> Doc
params x = parens $ hcat $ prettifyList x

prettifyList :: Pretty a => [a] -> [Doc]
prettifyList [] = []
prettifyList [x] = [pretty x]
prettifyList (x:xs) = [pretty x <> comma <+> empty] ++ prettifyList xs

---------------------------

instance Pretty JSDouble where
  pretty (JSDouble x) = pretty x

instance Pretty JSFloat where
  pretty (JSFloat x) = pretty x

instance PrettyPrec JSDouble

instance Pretty JSVarStatement where
  pretty (JSVarStatement x) = (hcat $ prettifyList x) <> semi

instance PrettyPrec JSVarStatement

instance Pretty JSVarDecl where
  pretty (JSVarDecl x Nothing)  = text "var" <+> pretty x
  pretty (JSVarDecl x (Just e)) = text "var" <+> pretty x <+> text "=" <+> pretty e

instance PrettyPrec JSVarDecl

instance Pretty JSStatement where
  pretty (JSStatementExpression  es) = pretty es

instance PrettyPrec JSStatement -- default

instance Pretty JSExpressionStatement where
  pretty (JSESApply lvalues rvalue) =
    sepWith' (space <> text "=" <> space) lvalues <> pretty rvalue <> semi

instance Pretty JSLValue              where
  pretty (JSLValue name invsAndRefines) = pretty name <> (hcat . map ppIR $ invsAndRefines)
    where
      ppIR (invs, refine) = (hcat . map pretty $ invs) <> pretty refine

instance PrettyPrec JSLValue -- default

instance Pretty JSRValue              where
  pretty (JSRVInvoke invs) = hcat . toList . fmap pretty $ invs
  pretty (JSRVRefinement)  = text ""

instance PrettyPrec JSRValue -- default

instance Pretty JSExpression where
  pretty e = case e of
    JSExpressionLiteral literal      -> pretty literal
    JSExpressionName name            -> pretty name
    JSExpressionInvocation x i       -> pretty x <> pretty i
    JSExpressionRefinement x r       -> pretty x <> pretty r
    JSExpressionNew x i              -> text "new" <+> pretty x <> pretty i

instance Pretty JSInvocation          where
  pretty (JSInvocation x)  = params x

instance PrettyPrec JSInvocation -- default

instance Pretty JSRefinement          where
  pretty (JSProperty a) = char '.' <> pretty a
  pretty (JSSubscript e)   = char '[' <> pretty e <> char ']'

instance PrettyPrec JSRefinement -- default

instance Pretty JSLiteral             where
  pretty lit = case lit of
    JSLiteralDouble x   -> pretty x
    JSLiteralFloat x    -> pretty x
    JSLiteralString s   -> pretty s
    JSLiteralObject o   -> pretty o
    JSLiteralArray  a   -> text "[" <+> pretty a <+> text "]"
    JSLiteralFunction f -> pretty f

instance PrettyPrec JSLiteral -- default

instance Pretty JSObjectLiteral       where
  pretty (JSObjectLiteral f) =
    lbrace <$>
    indent 2 (sepWith comma (map pretty f)) <$>
    rbrace

instance PrettyPrec JSObjectLiteral -- default

instance Pretty JSObjectField         where
  pretty (JSObjectField eitherNameString e) = ppEitherNameString <> colon <+> pretty e
    where ppEitherNameString = either pretty pretty eitherNameString

instance PrettyPrec JSObjectField -- default

instance Pretty JSArrayLiteral        where
  pretty (JSArrayLiteral es) = sepWith (comma <+> empty) es

instance PrettyPrec JSArrayLiteral -- default

instance Pretty JSFunctionLiteral     where
  pretty (JSFunctionLiteral mbName p body) =
    text "function" `join` (params p) <+> pretty body
      where join = case mbName of
                     Just name -> (\a b -> a <+> pretty name <> b)
                     Nothing   -> (<>)

instance PrettyPrec JSFunctionLiteral -- default

instance Pretty JSFunctionBody        where
  pretty (JSFunctionBody varStmts stmts) =
    lbrace <$>
    indent 2 (sepWith empty (map pretty varStmts ++ map pretty stmts)) <$>
    rbrace

instance PrettyPrec JSFunctionBody -- default
