{-# LANGUAGE FlexibleInstances #-}
module Structures where
import           Data.IORef
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust, isJust)
import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass

type VarName = String
type FunArgs = [VarList]
type VarDefs = [VarList]
type FunDefs = [FunDef]


instance Show (IORef ReturnValue) where
  show _ = ""

data ReturnValue
  = MLit Literal
  | MFun FunDef
  | MLambda FunDef (Map.Map VarName (IORef ReturnValue))
  | MArray (Map.Map Integer ReturnValue)
  | Void
  deriving Show

data Program
  = Program VarName VarDefs FunDefs Statement
  deriving (Show, Eq)

data FunDef
  = FunDef FunHeader VarDefs FunDefs Statement
  deriving (Show, Eq)

data VarList
  = VarList [Variable]
  deriving (Show, Eq)

data Variable
  = Variable VarName (Maybe Expr) VarType
  deriving (Show, Eq)

data FunHeader
  = FunHeader (Maybe VarName) FunArgs VarType
  deriving (Show, Eq)

data VarType
  = TInt
  | TBool
  | TRec [VarType] VarType
  | TArray VarType Integer
  | Any
  deriving (Eq, Show)


data Statement
  = Seq [Statement]
  | If Expr Statement (Maybe Statement)
  | While Expr Statement
  | For ForHeader Statement
  | Assign VarCall AssignOp Expr
  | JustExpr Expr
  | BuiltInMethod BIM
  | Return Expr
  deriving (Show, Eq)

data ForHeader =
  ForHeader Statement Expr Statement
  deriving (Show, Eq)


data BIM
  = Print Expr
  -- | ToBool Expr
  -- | ToInt Expr
  deriving (Show, Eq)

data Expr
  = Lit Literal
  | Var VarCall
  | Lambda FunDef
  | Array [Expr]
  | Negate Expr
  | Not Expr
  | NumExpr NumBinOp Expr Expr
  | BoolExpr BoolBinOp Expr Expr
  | RelExpr RelBinOp Expr Expr
  deriving (Show, Eq)

data Literal
  = NumLit Integer
  | BoolLit Bool
  deriving (Show, Eq)

data VarCall
  = Single VarName
  | Fun VarCall [Expr]
  | Arr VarCall Expr
  deriving (Show, Eq)

data AssignOp
  = AMutate
  | AAdd
  | AMinus
  | AMultiply
  | ADivide
  | APower
  | AAnd
  | AOr
  | AXor
  deriving (Show, Eq)

data NumBinOp
  = Plus
  | Minus
  | Multiply
  | Divide
  | Power
  deriving (Show, Eq)

data BoolBinOp
  = And
  | Or
  | Xor
  deriving (Show, Eq)

data RelBinOp
  = Greater
  | Less
  | Equal
  | NotEqual
  | GreaterEqual
  | LessEqual
  deriving (Show, Eq)

----------PRETTY INSTANCES----------
instance Pretty NumBinOp where
  pPrint Plus     = text "+"
  pPrint Minus    = text "-"
  pPrint Multiply = text "*"
  pPrint Divide   = text "/"
  pPrint Power    = text "**"

instance Pretty BoolBinOp where
  pPrint And = text "&&"
  pPrint Or  = text "||"
  pPrint Xor = text "^"

instance Pretty RelBinOp where
  pPrint Greater      = text ">"
  pPrint Less         = text "<"
  pPrint Equal        = text "=="
  pPrint NotEqual     = text "<>"
  pPrint GreaterEqual = text ">="
  pPrint LessEqual    = text "<="

instance Pretty BIM where
  pPrint (Print e) =
    hcat [
      text "print(",
      pPrint e,
      text ")"
    ]

instance Pretty Expr where
  pPrint (Lit l)             = pPrint l
  pPrint (Lambda f)          = pPrint f
  pPrint (Array exs)         =
    brackets . hcat $ punctuate (comma <> space) (fmap pPrint exs)
  pPrint (Var v)             = pPrint v
  pPrint (Not expr)          = text "!" <> pPrint expr
  pPrint (Negate expr)       = text "-" <> pPrint expr
  pPrint (NumExpr op e1 e2)  =
    hcat [
      text "(",
      pPrint e1 <> pPrint op <> pPrint e2,
      text ")"
    ]
  pPrint (BoolExpr op e1 e2) =
    hcat [
      text "(",
      pPrint e1 <> pPrint op <> pPrint e2,
      text ")"
    ]
  pPrint (RelExpr op e1 e2)  =
    hcat [
      text "(",
      pPrint e1 <> pPrint op <> pPrint e2,
      text ")"
    ]

instance Pretty Literal where
  pPrint (NumLit l)  = pPrint l
  pPrint (BoolLit l) = pPrint l

instance Pretty VarCall where
  pPrint (Single name) = text name
  pPrint (Fun name args) =
    hcat [
      pPrint name,
      parens (hcat $ punctuate (comma <> space) (fmap pPrint args))
    ]
  pPrint (Arr name arg) =
    hcat [
      pPrint name,
      brackets (pPrint arg)
    ]

instance Pretty Statement where
  pPrint (Seq s) = vcat $ fmap ((<> text ";") . pPrint) s
  pPrint (If expr s elseS) =
    vcat [
      text "if(" <> pPrint expr <> text ") then",
      text "{",
      nest 1 (pPrint s),
      text "}",
      if isJust elseS
        then text "else {" <> pPrint (fromJust elseS) <> text "}"
        else text ""
    ]
  pPrint (JustExpr e) = pPrint e
  pPrint (While expr s) =
    vcat [
      text "while(" <> pPrint expr <> text ") do",
      text "{",
      nest 1 (pPrint s),
      text "}"
    ]
  pPrint (For fh s) =
    vcat [
      pPrint fh,
      text "{",
      nest 1 (pPrint s),
      text "}"
    ]
  pPrint (Assign s op e) = hcat [pPrint s, pPrint op, pPrint e]
  pPrint (BuiltInMethod b) = pPrint b
  pPrint (Return e) = text "return " <> pPrint e

instance Pretty ForHeader where
  pPrint (ForHeader e1 bExpr e2) =
    hcat [
      text "for(",
      pPrint e1,
      text "; " <> pPrint bExpr <> pPrint "; ",
      pPrint e2,
      text ") do"
    ]

instance Pretty AssignOp where
  pPrint AMutate   = text ":="
  pPrint AAdd      = text "+="
  pPrint AMinus    = text "-="
  pPrint AMultiply = text "*="
  pPrint ADivide   = text "/="
  pPrint APower    = text "**="
  pPrint AAnd      = text "&="
  pPrint AOr       = text "|="
  pPrint AXor      = text "^="

instance Pretty VarList where
  pPrint (VarList e) = pPrint e

instance Pretty Variable where
  pPrint (Variable name Nothing t) =
    hcat [
      text name,
      colon,
      pPrint t
    ]
  pPrint (Variable name (Just v) t) =
    hcat [
      text name,
      text " := ",
      pPrint v,
      colon,
      pPrint t
    ]

instance Pretty ReturnValue where
  pPrint (MLit l) = pPrint l
  pPrint (MFun f) = pPrint f
  pPrint Void     = error "Variable is undefined"
  pPrint (MLambda f _) = pPrint f
  pPrint (MArray m) = pPrint $ Map.elems m

instance Pretty VarType where
  pPrint TInt       = text "int"
  pPrint TBool      = text "bool"
  pPrint (TRec t r) =
    hcat [
      parens . hcat $ punctuate (comma <> space) (fmap pPrint t),
      text " -> ",
      pPrint r
    ]
  pPrint (TArray t i) = brackets $ pPrint t <> text " | " <> pPrint i
  pPrint Any = text "*"

instance Pretty FunDef where
  pPrint (FunDef fh vdefs fdefs body) =
    vcat [
      pPrint fh,
      nest 2 (vcat $ fmap pPrint vdefs),
      nest 2 (vcat $ fmap pPrint fdefs),
      nest 2 (text "begin"),
      nest 2 (pPrint body),
      nest 2 (text "end")
    ]
instance Pretty FunHeader where
  pPrint (FunHeader name args t) =
    hcat [
      text "function ",
      maybe (text "") text name,
      parens . hcat $ punctuate (semi <> space) (fmap pPrint args),
      colon,
      pPrint t
    ]
instance Pretty Program where
  pPrint (Program name vdefs fdefs body) =
    vcat [
      text "program " <> text name <> text ";",
      nest 2 (vcat $ fmap pPrint vdefs),
      nest 2 (vcat $ fmap pPrint fdefs),
      nest 2 (text "begin"),
      nest 2 (pPrint body),
      nest 2 (text "end")
    ]
