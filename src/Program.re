type procedure =
  | Plus
  | Minus;
type primitive =
  | Int(int)
  | Float(float)
  | String(string)
  | Bool(bool);

type token =
  | Primitive(primitive)
  | Symbol(string)
  | Variable(string)
  | Procedure(procedure);

type term =
  | Token(token)
  | Abstraction(string, term)
  | Application(term, term);