open Terms;
[@react.component]
type action =
  | Tick;
type state = {term};
let t =
  Application(
    Abstraction("y", Application(Token("x"), Token("y"))),
    Token("z"),
  );
let y =
  Abstraction(
    "f",
    Application(
      Abstraction(
        "x",
        Application(Token("f"), Application(Token("x"), Token("x"))),
      ),
      Abstraction(
        "x",
        Application(Token("f"), Application(Token("x"), Token("x"))),
      ),
    ),
  );
let yInUse = Application(y, Abstraction("z", Token("z")));

let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Tick => {term: betaReduce(state.term)}
        },
      {term: yInUse},
    );
  React.useEffect0(() => {
    let timerId = Js.Global.setInterval(() => dispatch(Tick), 1000);
    Some(() => Js.Global.clearInterval(timerId));
  });
  <div> {termsToEl(state.term)} </div>;
};