open Program;
type result =
  | Result(term)
  | NoResult(term);
type proc =
  | NotProc
  | Proc(procedure, list(term));

exception HeadProcException;

let procToString = (p: procedure) =>
  switch (p) {
  | Plus => "+"
  | Minus => "-"
  };

let rec flattenProcBranch = (t: term) =>
  switch (t) {
  | Application(t1, t2) =>
    List.concat([flattenProcBranch(t1), flattenProcBranch(t2)])
  | _ => [t]
  };
let isProcBranch = (t: term) =>
  switch (t) {
  | Application(Token(Procedure(_)), _) => true
  | _ => false
  };

let makeProcBranch = (t: term) =>
  isProcBranch(t)
    ? {
      switch (flattenProcBranch(t)) {
      | [Token(Procedure(p)), ...rest] => Proc(p, rest)
      | _ => raise(HeadProcException)
      };
    }
    : NotProc;

let eval = (t: term) =>
  switch (makeProcBranch(t)) {
  | Proc(p, args) =>
    switch (p, args) {
    | (Plus, [Token(Primitive(Int(a))), Token(Primitive(Int(b)))]) =>
      Result(Token(Primitive(Int(a + b))))
    | _ => NoResult(t)
    }
  | NotProc => NoResult(t)
  };

let rec evalHelper = (t: result) =>
  switch (t) {
  | Result(t) => Result(t)
  | NoResult(Application(t1, t2)) =>
    switch (eval(Application(t1, t2))) {
    | Result(t) => Result(t)
    | NoResult(_) =>
      switch (evalHelper(NoResult(t1)), evalHelper(NoResult(t2))) {
      | (Result(r1), NoResult(r2)) => Result(Application(r1, r2))
      | (NoResult(r1), Result(r2)) => Result(Application(r1, r2))
      // Comment out next line for parallel eval
      // | (Result(r1), Result(r2)) => Result(Application(r1, r2))
      | (Result(r1), Result(_)) => NoResult(Application(r1, t2))
      | (NoResult(r1), NoResult(r2)) => NoResult(Application(r1, r2))
      }
    }
  | NoResult(Abstraction(x, t1)) =>
    let e = evalHelper(NoResult(t1));
    switch (e) {
    | Result(r1) => Result(Abstraction(x, r1))
    | NoResult(r1) => NoResult(Abstraction(x, r1))
    };
  | NoResult(Token(t1)) => NoResult(Token(t1))
  };