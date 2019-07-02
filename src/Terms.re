type term =
  | Token(string)
  | Abstraction(string, term)
  | Application(term, term);

let rec termsToString = (t: term) => switch (t) {
  | Token(s) => s
  | Abstraction(s, t_) => "(\\" ++ s ++ " -> " ++ termsToString(t_) ++ ")"
  | Application(t1, t2) => "(" ++ termsToString(t1) ++ " " ++ termsToString(t2) ++ ")"
};