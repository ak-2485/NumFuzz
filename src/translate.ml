type symbol = string

type fpcore = FPCore of (symbol * argument list * property list * expr)
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
