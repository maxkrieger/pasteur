type procedure =
  | Plus
  | Minus;
type primitive =
  | Int(int)
  | String(string);

type variable = (string, ref(primitive));
type env = list(variable);

type token =
  | Primitive(primitive)
  | Symbol(string)
  | Variable(string)
  | Procedure(procedure);

type term =
  | Token(token)
  | Abstraction(string, term)
  | Application(term, term);