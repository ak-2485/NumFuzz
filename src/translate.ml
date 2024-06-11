open Support.FileInfo

type symbol = string

type fpcore = FPCore of (symbol * argument list * property list * expr) | TEST
and dimension = Num of int | DSymbol of symbol
and argument = ASymbol of symbol | Array of (symbol * dimension list)

and expr =
  | ENum of float
  | ESymbol of symbol
  | OP of (op * expr)
  | If of (expr * expr * expr)
  | Let of symbol * expr * expr
  | EArray of expr list

and op = Plus | Times | Divide | Sqrt | Equals | LessThan
and property

type program = fpcore list

let get_name (inf : info) = match inf with FI (sym, _, _) -> sym | UNKNOWN -> ""

let rec translate (prog : Syntax.term) : program = match prog with 
| TmAbs (inf, bind, typ, func) -> [(translate_function "" (bind.b_name) typ func)]
| TmLet (inf, bind, typ, t1, t2) -> [(translate_function (get_name inf) (bind.b_name) typ t1)] @ translate t2
| _ -> [TEST]


and translate_function (name : symbol) (arg : string) (typ : Syntax.ty) (exp : Syntax.term) : fpcore = TEST
