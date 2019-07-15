open Program;
let primitive_to_string = (p: primitive) =>
  switch (p) {
  | Int(i) => string_of_int(i)
  | String(s) => s
  };

let rec termsToEl = (t: term) =>
  switch (t) {
  | Token(s) =>
    let c =
      switch (s) {
      | Primitive(p) => primitive_to_string(p)
      | Symbol(s) => s
      | Variable(v) => v
      | Procedure(p) => NativeProcedures.procToString(p)
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
  | Application(Token(Procedure(p)), t2) =>
    <span>
      {ReasonReact.string("(")}
      {termsToEl(Token(Procedure(p)))}
      {ReasonReact.string(" ")}
      {termsToEl(t2)}
      {ReasonReact.string(")")}
    </span>

  | Application(t1, t2) =>
    <span> {termsToEl(t1)} {ReasonReact.string(" ")} {termsToEl(t2)} </span>
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

// To evaluate depth-wise, use https://reasonml.github.io/docs/en/imperative-loops#tips-tricks
let eval = (t: term) => {
  let stepped = ref(false);
  let e = ref(t);
  while (! stepped^) {
    switch (NativeProcedures.evalHelper(NoResult(e^))) {
    | NativeProcedures.Result(r) =>
      e := r;
      stepped := true;
    | NativeProcedures.NoResult(r) =>
      if (hasEta(r)) {
        e := etaConvert(r);
        stepped := true;
      } else if (hasRedex(r)) {
        e := betaReduce(r);
        stepped := true;
      } else {
        e := r;
        stepped := true;
      }
    };
  };
  e^;
};