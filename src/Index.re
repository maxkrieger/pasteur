open Terms;
let t = Abstraction("y", Application(Token("x"), Token("y")));
ReactDOMRe.renderToElementWithId(<TermNode term=t />, "index1");
