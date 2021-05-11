import Array "mo:base/Array";
import Int "mo:base/Int";
import List "mo:base/List";
import Debug "mo:base/Debug";
import P "../src/Pretty";

type Json = {
    #number : Int;
    #string : Text;
    #jnull;
    #array : [Json];
    #obj : [(Text, Json)]
};

func jNumber(i : Int) : Json = #number(i);
func jString(s : Text) : Json = #string(s);
func jArray(a : [Json]) : Json = #array(a);
func jObj(o : [(Text, Json)]) : Json = #obj(o);

let trailingComma : P.Doc<None> =
  P.flexAlt (P.text ", ", P.append(P.text ",", P.lineBreak()));

func jsCurlies<A>(doc : P.Doc<A>) : P.Doc<A> {
  let open = P.flexAlt(P.text("{"), P.append(P.text("{"), P.lineBreak()));
  let close = P.flexAlt(P.text("}"), P.append(P.lineBreak(), P.text("}")));
  P.flexGroup(
    P.encloseEmptyAlt(
      open,
      close,
      P.text("{}"),
      P.indent(doc)
    )
  )
};

func jsBrackets<A>(doc : P.Doc<A>) : P.Doc<A> {
  let open = P.flexAlt(P.text("["), P.append(P.text("["), P.lineBreak()));
  let close = P.flexAlt(P.text("]"), P.append(P.lineBreak(), P.text("]")));
  P.flexGroup(
    P.encloseEmptyAlt(
      open,
      close,
      P.text("[]"),
      P.indent(doc)
    )
  )
};

func prettyJson(json : Json) : Text {
  func go(json : Json) : P.Doc<None> {
    switch json {
      case (#number(n)) {
        P.text(Int.toText(n))
      };
      case (#string(s)) {
        P.enclose(P.text("\""), P.text("\""), P.text(s))
      };
      case (#jnull) {
        P.text("null")
      };
      case (#array(elements)) {
        jsBrackets(P.foldWithSeparator(trailingComma, Array.map(elements, go)))
      };
      case (#obj(elements)) {
        let docs = Array.map(elements, func ((k : Text, v : Json)) : P.Doc<None> {
          P.appendAll([
            P.enclose(P.text("\""), P.text("\""), P.text(k)),
            P.text ": ",
            go(v)
          ].vals())
        });
        jsCurlies(P.foldWithSeparator(trailingComma, docs))
      };
    }
  };
  P.print(P.plainText, P.twoSpaces, go(json))
};

let j1 = jArray([
  jNumber(10), jNumber(20), jString("hi dude"), jNumber(30), jNumber(40),
  jNumber(100000), jNumber(100000), jNumber(100000), jNumber(100000)
]);
let j2 = jObj([
  ("Hello", jNumber(10)), ("MyDude", jNumber(20)), ("What a wonderful day", jString("hi dude")),
  ("Hello2", jNumber(10)), ("MyDude2", jNumber(20)), ("What a wonderful day2", j1),
]);

Debug.print(prettyJson(j1));
Debug.print(prettyJson(j2));
