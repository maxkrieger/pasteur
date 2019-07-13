open Terms;
type action =
  | Tick;
type state = {term};
let t =
  Application(
    Abstraction(
      "y",
      Application(Token(Variable("x")), Token(Variable("y"))),
    ),
    Token(Variable("z")),
  );
let y =
  Abstraction(
    "f",
    Application(
      Abstraction(
        "x",
        Application(
          Token(Variable("f")),
          Application(Token(Variable("x")), Token(Variable("x"))),
        ),
      ),
      Abstraction(
        "x",
        Application(
          Token(Variable("f")),
          Application(Token(Variable("x")), Token(Variable("x"))),
        ),
      ),
    ),
  );
let yInUse = Application(y, Abstraction("z", Token(Variable("z"))));

let etaInUse =
  Abstraction(
    "x",
    Application(
      Abstraction("z", Token(Variable("z"))),
      Token(Variable("x")),
    ),
  );

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Tick => {term: eval(state.term)}
        },
      {term: etaInUse},
    );
  React.useEffect0(() => {
    let timerId = Js.Global.setInterval(() => dispatch(Tick), 1000);
    Some(() => Js.Global.clearInterval(timerId));
  });
  <div> {termsToEl(state.term)} </div>;
};