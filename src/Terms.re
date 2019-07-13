type variable = (string, ref(unit));
type env = list(variable);

type token =
  | Primitive(ref(unit))
  | Symbol(string)
  | Variable(string)
  | Procedure(string);

type term =
  | Token(token)
  | Abstraction(string, term)
  | Application(term, term);

let rec termsToEl = (t: term) =>
  switch (t) {
  | Token(s) =>
    let c =
      switch (s) {
      | Primitive(_) => "val"
      | Symbol(s) => s
      | Variable(v) => v
      | Procedure(p) => p
      };
    <span style={ReactDOMRe.Style.make(~color="green", ())}>
      {ReasonReact.string(c)}
    </span>;
  | Abstraction(s, t_) =>
    <span>
      {ReasonReact.string("(\\" ++ s ++ " -> ")}
      {termsToEl(t_)}
      {ReasonReact.string(")")}
    </span>
  | Application(t1, t2) =>
    <span>
      {ReasonReact.string("(")}
      {termsToEl(t1)}
      {ReasonReact.string(" ")}
      {termsToEl(t2)}
      {ReasonReact.string(")")}
    </span>
  };

let rec substitution = (body: term, placeholder: string, substitute: term) =>
  switch (body) {
  | Token(Variable(s)) => s == placeholder ? substitute : Token(Variable(s))
  | Abstraction(s, t) =>
    s == placeholder
      ? substitution(t, placeholder, substitute)
      : Abstraction(s, substitution(t, placeholder, substitute))
  | Application(t1, t2) =>
    Application(
      substitution(t1, placeholder, substitute),
      substitution(t2, placeholder, substitute),
    )
  | _ => body
  };

let etaConvert = (t: term) =>
  switch (t) {
  | Abstraction(s1, Application(t1, Token(Variable(s2)))) =>
    s1 == s2 ? t1 : t
  | _ => t
  };

let rec hasEta = (t: term) =>
  switch (t) {
  | Abstraction(s1, Application(_, Token(Variable(s2)))) => s1 == s2
  | Abstraction(_, t1) => hasEta(t1)
  | Application(t1, t2) => hasEta(t1) || hasEta(t2)
  | Token(_) => false
  };

let rec hasRedex = (t: term) =>
  switch (t) {
  | Application(Abstraction(_, _), _) => true
  | Application(t1, t2) => hasRedex(t1) || hasRedex(t2)
  | Abstraction(_, t1) => hasRedex(t1)
  | Token(_) => false
  };

let rec betaReduce = (t: term) =>
  switch (t) {
  | Application(Abstraction(placeholder, body), substitute) =>
    substitution(body, placeholder, substitute)
  | Application(Application(t1, t2), substitute) =>
    Application(betaReduce(Application(t1, t2)), substitute)
  | _ => t
  };

let eval = (t: term) =>
  switch (t) {
  | Application(Token(Procedure(_)), Token(_)) => Token(Symbol("yay"))
  | _ => hasEta(t) ? etaConvert(t) : hasRedex(t) ? betaReduce(t) : t
  };