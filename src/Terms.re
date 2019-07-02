type term =
  | Token(string)
  | Abstraction(string, term)
  | Application(term, term);

// TODO: Use de bruijn or alpha to ensure var uniqueness
// and avoid call binding

let rec termsToEl = (t: term) =>
  switch (t) {
  | Token(s) =>
    <span style={ReactDOMRe.Style.make(~color="green", ())}>
      {ReasonReact.string(s)}
    </span>
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
  | Token(s) => s == placeholder ? substitute : Token(s)
  | Abstraction(s, t) =>
    s == placeholder
      ? substitution(t, placeholder, substitute)
      : Abstraction(s, substitution(t, placeholder, substitute))
  | Application(t1, t2) =>
    Application(
      substitution(t1, placeholder, substitute),
      substitution(t2, placeholder, substitute),
    )
  };
let rec betaReduce = (t: term) =>
  switch (t) {
  | Token(s) => Token(s)
  | Application(Token(t1), t2) => Application(Token(t1), t2)
  | Application(Abstraction(placeholder, body), substitute) =>
    substitution(body, placeholder, substitute)
  | Application(Application(t1, t2), substitute) =>
    Application(betaReduce(Application(t1, t2)), substitute)
  | Abstraction(s, t) => Abstraction(s, t)
  };