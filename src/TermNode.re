open Program;
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

let plusInUse =
  Application(
    Application(
      Token(Procedure(Plus)),
      Application(
        Application(
          Token(Procedure(Plus)),
          Application(
            Token(Primitive(Int(2))),
            Token(Primitive(Int(3))),
          ),
        ),
        Application(
          Token(Procedure(Plus)),
          Application(
            Token(Primitive(Int(1))),
            Token(Primitive(Int(2))),
          ),
        ),
      ),
    ),
    Token(Primitive(String("foo"))),
  );

// let parse_error lexbuf =
// syntax_error (
// if lexbuf.Lexing.lex_curr_pos == lexbuf.Lexing.lex_last_pos then
// "Unexpected end of file"
// else
// ("Unexpected token '" ^ (Lexing.lexeme lexbuf) ^ "'")
// ) (Lexing.lexeme_start_p lexbuf)
// ;

exception Parse_Error;
let try_parse = lexbuf =>
  try (Parser.main(Lexer.token(lexbuf))) {
  | _ => raise(Parse_Error)
  };

let string = str => try_parse(Lexing.from_string(str));

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Tick => {term: Terms.eval(state.term)}
        },
      {term: plusInUse},
    );
  React.useEffect0(() => {
    let timerId = Js.Global.setInterval(() => dispatch(Tick), 1000);
    Some(() => Js.Global.clearInterval(timerId));
  });
  <div> {Terms.termsToEl(state.term)} </div>;
};