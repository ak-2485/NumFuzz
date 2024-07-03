exception ElementaryOperation of string

type symbol = string
type precision = Binary64 | Binary32 | Binary16 | Real
type translate_flag = Default | NaiveInline | SmartInline | Decimal

type fpcore = FPCore of (symbol option * argument list * property list * expr)
and dimension = int
and argument = ASymbol of symbol | Array of (symbol * dimension list)

and expr =
  | EFloat of float
  | EInt of int
  | ESymbol of symbol
  | EOP of (fpop * expr list)
  | EIf of (expr * expr * expr)
  | ELet of (symbol * expr) list * expr
  | EArray of expr list
  | ERef of expr * expr list
  | EConstant of constant
  | EApp of expr * expr
  | EBang of property list * expr
  | ETensor of symbol * expr * (symbol * expr * expr) list * expr
  | EFor of symbol * expr * (symbol * expr * expr) list * expr

and data = DSymbol of symbol | DNum of float
and fpop = Plus | Times | Divide | Sqrt | Equals | GreaterThan | Cast
and constant = True | False
and property = Prec of precision | PRound

type program = fpcore list
