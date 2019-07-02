open Terms;
[@react.component]
let make = (~term) =>
  <div> {ReasonReact.string(termsToString(term))} </div>;
