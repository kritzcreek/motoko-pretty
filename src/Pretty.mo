/// A pretty printing library
///
/// Make it easy and fun to use your new library by including some module specific documentation here.
/// It's always a good idea to include a minimal working example:
///
/// ```motoko
/// import LibraryTemplate "mo:library-template/Library";
///
/// assert(LibraryTemplate.isPalindrome("anna"));
/// assert(not LibraryTemplate.isPalindrome("christoph"));
/// ```

import Array "mo:base/Array";
import Buffer "../internal/Buffer";
import Char "mo:base/Char";
import Float "mo:base/Float";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import List "mo:base/List";
import Nat "mo:base/Nat";
import Text "mo:base/Text";

module {

/// Document lines and columns are 0-based offsets.
type Position = {
  line : Nat;
  column : Nat;
  indent : Nat;
  nextIndent : Nat;
  pageWidth : Nat;
  ribbonWidth : Nat;
};

func setColumn(pos : Position, col : Nat) : Position {
  {
    line = pos.line;
    column = col;
    indent = pos.indent;
    nextIndent = pos.nextIndent;
    pageWidth = pos.pageWidth;
    ribbonWidth = pos.ribbonWidth;
  }
};

/// Documents are built using `append` as horizontal, line-wise concatenation.
/// The functions in this module let you build documents that respond well
/// to width constraints (such as `flexGroup` and `flexAlt`).
type Doc<A> = {
  #append : (Doc<A>, Doc<A>);
  #indent : Doc<A>;
  #align : (Nat, Doc<A>);
  #annotate : (A, Doc<A>);
  #flexSelect : (Doc<A>, Doc<A>, Doc<A>);
  #flexAlt : (Doc<A>, Doc<A>);
  #withPosition : Position -> Doc<A>;
  #text : (Nat, Text);
  #linebreak;
  #empty;
};

public func showDoc<A>(showAnn : A -> Text, doc : Doc<A>) : Text {
  switch doc {
    case (#append(d1, d2)) {
      "#append(" # showDoc(showAnn, d1) # ", " # showDoc(showAnn, d2) # ")"
    };
    case (#indent(d)) {
      "#indent(" # showDoc(showAnn, d) # ")"
    };
    case (#align(n, d)) {
      "#align(" # Nat.toText(n) # ", " # showDoc(showAnn, d) # ")"
    };
    case (#annotate(ann, d)) {
      "#annotate(" # showAnn(ann) # ", " # showDoc(showAnn, d) # ")"
    };
    case (#flexSelect(d1, d2, d3)) {
      "#flexSelect(" # showDoc(showAnn, d1) # ", " # showDoc(showAnn, d2) # ", " # showDoc(showAnn, d3) # ")"
    };
    case (#flexAlt(d1, d2)) {
      "#flexAlt(" # showDoc(showAnn, d1) # ", " # showDoc(showAnn, d2) # ")"
    };
    case (#withPosition(_)) {
      "#withPosition(<func>)"
    };
    case (#text(_, t)) {
      "#text(" # t # ")"
    };
    case (#linebreak) {
      "#linebreak"
    };
    case (#empty) {
      "#empty"
    };
  }
};

/// Transforms all annotations in the given document, using the passed function
public func mapDoc<A, B>(doc : Doc<A>, f : A -> B): Doc<B> {
  switch doc {
    case (#append(d1, d2)) {
      #append(mapDoc(d1, f), mapDoc(d2, f))
    };
    case (#indent(d)) {
      #indent(mapDoc(d, f))
    };
    case (#align(n, d)) {
      #align(n, mapDoc(d, f))
    };
    case (#annotate(a, d)) {
      #annotate(f(a), mapDoc(d, f))
    };
    case (#flexSelect(d1, d2, d3)) {
      #flexSelect(
        mapDoc(d1, f),
        mapDoc(d2, f),
        mapDoc(d3, f)
      )
    };
    case (#flexAlt(d1, d2)) {
      #flexAlt(
        mapDoc(d1, f),
        mapDoc(d2, f)
      )
    };
    case (#withPosition(f1)) {
      #withPosition(func pos { mapDoc(f1(pos), f) })
    };
    case (#text(n, t)) {
      #text(n, t)
    };
    case (#linebreak) {
      #linebreak
    };
    case (#empty) {
      #empty
    };
  }
};

public func append<A>(d1: Doc<A>, d2: Doc<A>): Doc<A> {
  switch (d1, d2) {
    case (#empty, _) { d2 };
    case (_, #empty) { d1 };
    case (#text(n1, t1), #text(n2, t2)) { #text(n1 + n2, t1 # t2) };
    case (_, _) { #append(d1, d2) };
  }
};

public func bothNotEmpty<A>(d1: Doc<A>, d2: Doc<A>, f: (Doc<A>, Doc<A>) -> Doc<A>): Doc<A> {
  switch (d1, d2) {
    case (#empty, _) { d2 };
    case (_, #empty) { d1 };
    case (_, _) { f(d1, d2) };
  }
};

/// The empty document
public let empty: Doc<None> = #empty;

/// Checks whether the document is empty.
public func isEmpty<A>(doc: Doc<A>): Bool {
  switch (doc) {
    case (#empty) { true };
    case _ { false };
  }
};

/// Only applies the provided function if the document is non-empty.
func notEmpty<A>(doc : Doc<A>, f : Doc<A> -> Doc<A>) : Doc<A> {
  switch doc {
    case (#empty) { #empty };
    case _ { f(doc) };
  }
};

// The user-facing combinators

/// Increases the indentation level by one indent.
public func indent<A>(doc : Doc<A>) : Doc<A> {
  notEmpty<A>(doc, func d { #indent(d) })
};

/// Increases the indentation level by the number of spaces (for alignment purposes).
public func align<A>(n : Nat, doc : Doc<A>) : Doc<A> {
  if (n > 0) {
    notEmpty<A>(doc, func d { #align(n, d) })
  } else {
    doc
  }
};

/// Increases the indentation level so that it aligns to the current column.
public func alignCurrentColumn<A>(doc : Doc<A>) : Doc<A> {
  notEmpty<A>(doc, func d {
    withPosition<A>(func pos {
      let n : Nat = if (pos.column > pos.nextIndent) {
        pos.column - pos.nextIndent
      } else {
        0
      };
      align(n, doc)
    })
  })
};

/// Adds an annotation to a document. Printers can interpret annotations to style
/// their output, eg. ANSI colors.
public func annotate<A>(annotation : A, doc : Doc<A>) : Doc<A> {
  notEmpty<A>(doc, func d { #annotate(annotation, d) })
};

/// Attempts to layout the document with flex alternatives, falling back
/// to defaults if it doesn't fit the page width.
public func flexGroup<A>(doc : Doc<A>) : Doc<A> {
  notEmpty<A>(doc, func d {
    switch d {
      case(#flexSelect(_, a, b)) {
        if (isEmpty(a) and isEmpty(b)) {
          d
        } else {
          #flexSelect(doc, #empty, #empty)
        }
      };
      case _ { #flexSelect(doc, #empty, #empty) };
    }
  })
};

/// Attempts to layout the first document with flex alternatives, falling
/// back to defaults if it doesn't fit the page width. If the flex alternatives
/// are used then the second document will be appended, otherwise the third
/// document will be appended.
// TODO: Find good names for the parameters here
public func flexSelect<A>(doc : Doc<A>, defaults : Doc<A>, fallback : Doc<A>) : Doc<A> {
  if (isEmpty(doc)) {
    defaults
  } else {
    #flexSelect(doc, defaults, fallback)
  }
};

/// Attempts to layout the first document when in a flex group, falling back
/// to the second as a default.
public func flexAlt<A>(doc : Doc<A>, fallback : Doc<A>) : Doc<A> {
  #flexAlt(doc, fallback)
};

/// Build a document based on the current layout position.
public func withPosition<A>(f : Position -> Doc<A>) : Doc<A> {
  #withPosition(f)
};


/// The most basic document leaf. This should not contain newlines. If it does
/// your document will look very funny.
public func text<A>(t : Text) : Doc<A> {
  if (t == "") {
    #empty
  } else {
    #text(Text.size(t), t)
  }
};

/// Inserts a hard line break.
public func lineBreak<A>() : Doc<A> { #linebreak };

/// Inserts a space when in a flex group, otherwise inserts a break.
public func spaceBreak<A>() : Doc<A> { flexAlt(space(), lineBreak()) };

/// Inserts nothing when in a flex group, otherwise inserts a break.
public func softBreak<A>() : Doc<A> { flexAlt(#empty, lineBreak()) };

/// A singe space character.
public func space<A>() : Doc<A> { text(" ") };

/// Appends documents with a break in between them.
public func lines<A>(docs : [Doc<A>]) : Doc<A> {
  Array.foldRight<Doc<A>, Doc<A>>(docs, #empty, func (d1, d2) {
    // TODO: Ask why I can't eta reduce here?
      appendBreak(d1, d2)
    })
};

/// Appends documents with a space in between them.
public func words<A>(docs : [Doc<A>]) : Doc<A> {
  Array.foldRight<Doc<A>, Doc<A>>(docs, #empty, func (d1, d2) {
    // TODO: Ask why I can't eta reduce here?
      appendSpace(d1, d2)
    })
};

/// Appends documents with a space-break in between them.
public func paragraph<A>(docs : [Doc<A>]) : Doc<A> {
  Array.foldRight<Doc<A>, Doc<A>>(docs, #empty, func (d1, d2) {
    // TODO: Ask why I can't eta reduce here?
      appendSpaceBreak(d1, d2)
    })
};

let isWhitespace : Text.Pattern = #predicate(func c {
    Char.isWhitespace(c)
});

/// Constructs a wrapping paragraph from a blob of text. Ignores newlines and
/// multiple spaces.
public func textParagraph<A>(t : Text) : Doc<A> {
  let texts = Text.split(Text.trim(t, isWhitespace), isWhitespace);
  paragraph(Array.map<Text, Doc<A>>(Iter.toArray(texts), func t { text(t) }))
};

/// Appends two documents with a break between them.
public func appendBreak<A>(doc1 : Doc<A>, doc2 : Doc<A>) : Doc<A> {
  bothNotEmpty<A>(doc1, doc2, func (a, b) {
    append(a, append(lineBreak(), b))
  })
};

/// Appends two documents with a space between them.
public func appendSpace<A>(doc1 : Doc<A>, doc2 : Doc<A>) : Doc<A> {
  bothNotEmpty<A>(doc1, doc2, func (a, b) {
    append(a, append(space(), b))
  })
};

/// Appends two documents with a space between them, falling back to a
/// break if that does not fit.
public func appendSpaceBreak<A>(doc1 : Doc<A>, doc2 : Doc<A>) : Doc<A> {
  bothNotEmpty<A>(doc1, doc2, func (a, b) {
    append(a, flexGroup(append(space(), b)))
  })
};

/// Uses an opening and closing document to wrap another document.
public func enclose<A>(open : Doc<A>, close : Doc<A>, inner : Doc<A>) : Doc<A> {
  append(open, append(inner, close))
};

/// Uses an opening and closing document to wrap another document, falling
/// back when the inner document is empty.
/// ```motoko
/// encloseEmptyAlt(text("[ "), text(" ]"), text("[]"), #empty)
/// ```
public func encloseEmptyAlt<A>(open : Doc<A>, close : Doc<A>, fallback : Doc<A>, inner : Doc<A>) : Doc<A> {
  if (isEmpty(inner)) {
    fallback
  } else {
    append(open, append(inner, close))
  }
};

/// Uses an opening and closing document, as a well as a separator, to render
/// a series of documents.
public func encloseWithSeparator<A>(open : Doc<A>, close : Doc<A>, separator : Doc<A>, inner : [Doc<A>]) : Doc<A> {
  append(open, append(foldWithSeparator(separator, inner), close))
};

/// Appends a series of documents together with a separator in between them.
public func foldWithSeparator<A>(separator : Doc<A>, docs : [Doc<A>]) : Doc<A> {
  foldWith<A>(docs, func (d1, d2) { append(d1, append(separator, d2)) })
};

/// Appends a series of documents together with a given append function. This
/// is notable because it ignores empty documents.
public func foldWith<A>(docs : [Doc<A>], f : (Doc<A>, Doc<A>) -> Doc<A>) : Doc<A> {
  Array.foldRight<Doc<A>, Doc<A>>(docs, #empty, func(d1, d2) {
    bothNotEmpty(d1, d2, f)
  })
};

public type Printer<Buf, A, Res> = {
  emptyBuffer : Buf;
  writeText : (Nat, Text, Buf) -> Buf;
  writeIndent : (Nat, Text, Buf) -> Buf;
  writeBreak : Buf -> Buf;
  enterAnnotation : (A, List.List<A>, Buf) -> Buf;
  leaveAnnotation : (A, List.List<A>, Buf) -> Buf;
  flushBuffer : Buf -> Res;
};

/// A plain text printer. Can be used with any dument.
public let plainText : Printer<Text, Any, Text> = {
  emptyBuffer = "";
  writeText = func(_, str, buf) { buf # str };
  writeIndent = func(_, str, buf) { buf # str };
  writeBreak = func(buf) { buf # "\n" };
  enterAnnotation = func(_, _, buf) { buf };
  leaveAnnotation = func(_, _, buf) { buf };
  flushBuffer = func(buf) { buf };
};

/// Configuration options for the printer.
/// * `pageWidth` - The printer will try not to exceed this width on any given line.
/// * `ribbonRatio` - Ratio between 0.0 and 1.0, defaults to 1.0. The printer will
///   use this ratio to calculate the printable area between the current indentation
///   level and the `pageWidth`.
/// * `indentUnit` - The string used for a single indent.
/// * `indentWidth` - The assumed character width of a single `indentUnit`.
public type PrintOptions ={
  pageWidth : Nat;
  ribbonRatio : Float;
  indentUnit : Text;
  indentWidth : Nat;
};

/// Prints 2-space indents, with a default 80-column page width.
public let twoSpaces: PrintOptions = {
  pageWidth = 80;
  ribbonRatio = 1.0;
  indentUnit = "  ";
  indentWidth = 2;
};

/// Prints 4-space indents, with a default 80-column page width.
public let fourSpaces: PrintOptions = {
  pageWidth = 80;
  ribbonRatio = 1.0;
  indentUnit = "    ";
  indentWidth = 4;
};

type DocCmd<A> = {
  #doc : (Doc<A>);
  #dedent : (Text, Nat);
  #leaveAnnotation : (A, List.List<A>);
  #leaveFlexGroup : (Doc<A>, Doc<A>);
};

type FlexGroupState<B, A> = {
  position : Position;
  buffer : Buffer.T<B>;
  annotations : List.List<A>;
  indentSpaces : Text;
  stack : List.List<DocCmd<A>>;
};

type FlexGroupStatus<B, A> = {
  #noFlexGroup;
  #flexGroupPending;
  #flexGroupReset : FlexGroupState<B, A>;
};

type DocState<B, A> = {
  position : Position;
  buffer : Buffer.T<B>;
  annotations : List.List<A>;
  indentSpaces : Text;
  flexGroup : FlexGroupStatus<B, A>;
};

func resetState<A, B>(st : FlexGroupState<B, A>) : DocState<B, A> {
  {
    position = st.position;
    buffer = st.buffer;
    annotations = st.annotations;
    indentSpaces = st.indentSpaces;
    flexGroup = #noFlexGroup;
  }
};

func storeState<A, B>(stack : List.List<DocCmd<A>>, st : DocState<B, A>) : FlexGroupState<B, A> {
  {
    position = st.position;
    buffer = st.buffer;
    annotations = st.annotations;
    indentSpaces = st.indentSpaces;
    stack;
  }
};

/// Prints a documents given a printer and print options.
///
/// This will use full line-lookahead from the start of a flex group. If it
/// encounters a break or content overflows the page-width, it will layout
/// the group using flex alternative defaults instead.
public func print<Buf, A, Res>(printer : Printer<Buf, A, Res>, opts : PrintOptions, doc : Doc<A>) : Res {
  let ribbonRatio : Float = Float.max(0.0, (Float.min(1.0, opts.ribbonRatio)));
  func calcRibbonWidth(x : Nat) : Nat {
    Int.abs(Int.max(0, Float.toInt(Float.ceil(ribbonRatio * Float.fromInt(opts.pageWidth - x)))))
  };

  let initialState: DocState<Buf, A> = {
    position = {
      line = 0;
      column = 0;
      indent = 0;
      nextIndent = 0;
      pageWidth = opts.pageWidth;
      ribbonWidth = calcRibbonWidth(0);
    };
    buffer = Buffer.new(printer.emptyBuffer);
    annotations = List.nil();
    indentSpaces = "";
    flexGroup = #noFlexGroup;
  };

  var stack: List.List<DocCmd<A>> = List.make(#doc(doc));

  func go(state : DocState<Buf, A>) : Res {
    switch stack {
      case null { printer.flushBuffer(Buffer.get(state.buffer)) };
      case (?(cmd, stk)) {
        switch cmd {
          case (#doc(doc)) {
            switch(doc) {
              case (#append(doc1, doc2)) {
                stack := List.push(#doc(doc1), List.push(#doc(doc2), stk));
                go(state)
              };
              case (#text(len, str)) {
                if (state.position.column == 0 and state.position.indent > 0) {
                  go({
                      annotations = state.annotations;
                      indentSpaces = state.indentSpaces;
                      flexGroup = state.flexGroup;
                      buffer = Buffer.modify(
                        state.buffer,
                        func (b: Buf): Buf {
                          printer.writeIndent(state.position.indent, state.indentSpaces, b)
                        });
                      position = setColumn(state.position, state.position.indent);
                    })
                } else if (state.position.column + len <= state.position.indent + state.position.ribbonWidth) {
                  stack := stk;
                  go({
                      annotations = state.annotations;
                      indentSpaces = state.indentSpaces;
                      flexGroup = state.flexGroup;
                      buffer = Buffer.modify(state.buffer, func (t : Buf) : Buf { printer.writeText(len, str, t)});
                      position = setColumn(state.position, state.position.column + len);
                    })
                } else {
                  switch (state.flexGroup) {
                    case (#flexGroupReset(frame)) {
                      stack := frame.stack;
                      go(resetState(frame))
                    };
                    case _ {
                      stack := stk;
                      go({
                        annotations = state.annotations;
                        indentSpaces = state.indentSpaces;
                        flexGroup = #noFlexGroup;
                        buffer = Buffer.modify(state.buffer, func (t : Buf) : Buf { printer.writeText(len, str, t) });
                        position = setColumn(state.position, state.position.column + len);
                      })
                    };
                  }
                }
              };
              case (#linebreak) {
                switch (state.flexGroup) {
                  case (#flexGroupReset(frame)) {
                    stack := frame.stack;
                    go(resetState(frame))
                  };
                  case _ {
                    stack := stk;
                    go({
                      annotations = state.annotations;
                      indentSpaces = state.indentSpaces;
                      flexGroup = #noFlexGroup;
                      buffer = Buffer.modify(state.buffer, printer.writeBreak);
                      position = {
                        line = state.position.line + 1;
                        column = 0;
                        indent = state.position.nextIndent;
                        ribbonWidth = calcRibbonWidth(state.position.nextIndent);
                        nextIndent = state.position.nextIndent;
                        pageWidth = state.position.pageWidth;
                      };
                    })
                  };
                }
              };
              case (#indent(doc)) {
                if (state.position.column == 0) {
                  stack := List.push(#doc(doc), List.push(#dedent(state.indentSpaces, state.position.nextIndent), stk));
                  go({
                    indentSpaces = state.indentSpaces # opts.indentUnit;
                    position = {
                      indent = state.position.nextIndent + opts.indentWidth;
                      ribbonWidth = calcRibbonWidth(state.position.nextIndent + opts.indentWidth);
                      nextIndent = state.position.nextIndent + opts.indentWidth;

                      line = state.position.line;
                      column = state.position.column;
                      pageWidth = state.position.pageWidth;
                    };

                    annotations = state.annotations;
                    flexGroup = state.flexGroup;
                    buffer = state.buffer;
                  })
                } else {
                  stack := List.push(#doc(doc), List.push(#dedent(state.indentSpaces, state.position.nextIndent), stk));
                  go({
                    indentSpaces = state.indentSpaces # opts.indentUnit;
                    position = {
                      nextIndent = state.position.nextIndent + opts.indentWidth;

                      line = state.position.line;
                      column = state.position.column;
                      indent = state.position.nextIndent;
                      ribbonWidth = state.position.ribbonWidth;
                      pageWidth = state.position.pageWidth;
                    };

                    annotations = state.annotations;
                    flexGroup = state.flexGroup;
                    buffer = state.buffer;
                  })
                }
              };
              case (#align(width, doc)) {
                if (state.position.column == 0) {
                  stack := List.push(#doc(doc), List.push(#dedent(state.indentSpaces, state.position.nextIndent), stk));
                  go({
                    indentSpaces = state.indentSpaces # Text.join("", Iter.fromList(List.replicate(width, " ")));
                    position = {
                      indent = state.position.nextIndent + width;
                      nextIndent = state.position.nextIndent + width;
                      ribbonWidth = calcRibbonWidth(state.position.nextIndent + width);

                      line = state.position.line;
                      column = state.position.column;
                      pageWidth = state.position.pageWidth;
                    };

                    annotations = state.annotations;
                    flexGroup = state.flexGroup;
                    buffer = state.buffer;
                  })
                } else {
                  stack := List.push(#doc(doc), List.push(#dedent(state.indentSpaces, state.position.nextIndent), stk));
                  go({
                    indentSpaces = state.indentSpaces # Text.join("", Iter.fromList(List.replicate(width, " ")));
                    position = {
                      nextIndent = state.position.nextIndent + width;

                      indent = state.position.indent;
                      ribbonWidth = state.position.ribbonWidth;
                      line = state.position.line;
                      column = state.position.column;
                      pageWidth = state.position.pageWidth;
                    };

                    annotations = state.annotations;
                    flexGroup = state.flexGroup;
                    buffer = state.buffer;
                  })
                }
              };
              case (#flexSelect(doc1, doc2, doc3)) {
                switch (state.flexGroup) {
                  case (#noFlexGroup) {
                    stack := List.push(#doc(doc1), List.push(#leaveFlexGroup(doc2, doc3), stk));
                    go({
                      flexGroup = #flexGroupPending;

                      annotations = state.annotations;
                      indentSpaces = state.indentSpaces;
                      buffer = state.buffer;
                      position = state.position;
                    })
                  };
                  case (#flexGroupPending) {
                    if (state.position.ribbonWidth > 0) {
                      let reset = #flexGroupReset(storeState(stack, state));
                      stack := List.push(#doc(doc1), List.push(#doc(doc2), stk));
                      go({
                        flexGroup = reset;
                        buffer = Buffer.branch(state.buffer);

                        annotations = state.annotations;
                        indentSpaces = state.indentSpaces;
                        position = state.position;
                      })
                    } else {
                      stack := List.push(#doc(doc1), List.push(#doc(doc2), stk));
                      go(state)
                    }
                  };
                  case _ {
                    stack := List.push(#doc(doc1), List.push(#doc(doc2), stk));
                    go(state)
                  };
                }
              };
              case (#flexAlt(flexDoc, doc1)) {
                switch (state.flexGroup) {
                  case (#flexGroupReset(_)) {
                    stack := List.push(#doc(flexDoc), stk);
                    go(state)
                  };
                  case (#flexGroupPending) {
                    if (state.position.ribbonWidth > 0) {
                      stack := List.push(#doc(flexDoc), stk);
                      go({
                        flexGroup = #flexGroupReset(storeState(List.push(#doc(doc1), stk), state));
                        buffer = Buffer.branch(state.buffer);

                        annotations = state.annotations;
                        indentSpaces = state.indentSpaces;
                        position = state.position;
                      })
                    } else {
                      stack := List.push(#doc(doc1), stk);
                      go(state)
                    }
                  };
                  case _ {
                    stack := List.push(#doc(doc1), stk);
                    go(state)
                  }
                }
              };
              case (#withPosition(k)) {
                if (state.position.column == 0 and state.position.nextIndent > 0) {
                  let newPos = {
                      column = state.position.nextIndent;

                      nextIndent = state.position.nextIndent;
                      line = state.position.line;
                      indent = state.position.nextIndent;
                      ribbonWidth = state.position.ribbonWidth;
                      pageWidth = state.position.pageWidth;
                  };
                  stack := List.push(#doc(k(newPos)), stk);
                  go(state)
                } else {
                  stack := List.push(#doc(k(state.position)), stk);
                  go(state)
                }
              };
              case (#annotate(ann, doc1)) {
                stack := List.push(#doc(doc1), List.push(#leaveAnnotation(ann, state.annotations), stk));
                go({
                  annotations = List.push(ann, state.annotations);
                  buffer = Buffer.modify(state.buffer, func(b : Buf): Buf {
                    printer.enterAnnotation(ann, state.annotations, b)
                  });

                  flexGroup = state.flexGroup;
                  indentSpaces = state.indentSpaces;
                  position = state.position;
                })
              };
              case (#empty) {
                stack := stk;
                go(state)
              };
            }
          };
          case (#leaveFlexGroup(doc1, doc2)) {
            switch (state.flexGroup) {
              case (#noFlexGroup) {
                stack := List.push(#doc(doc2), stk);
                go({
                  buffer = Buffer.commit(state.buffer);

                  annotations = state.annotations;
                  flexGroup = state.flexGroup;
                  indentSpaces = state.indentSpaces;
                  position = state.position;
                })
              };
              case _ {
                stack := List.push(#doc(doc1), stk);
                go({
                  buffer = Buffer.commit(state.buffer);
                  flexGroup = #noFlexGroup;

                  annotations = state.annotations;
                  indentSpaces = state.indentSpaces;
                  position = state.position;
                })
              };
            }
          };
          case (#dedent(indentSpaces, nextIndent)) {
            stack := stk;
            go({
              indentSpaces;
              position = {
                nextIndent;

                indent = state.position.indent;
                ribbonWidth = state.position.ribbonWidth;
                line = state.position.line;
                column = state.position.column;
                pageWidth = state.position.pageWidth;
              };

              annotations = state.annotations;
              flexGroup = state.flexGroup;
              buffer = state.buffer;
            })
          };
          case (#leaveAnnotation(ann, annotations)) {
            stack := stk;
            go({
                annotations;
                buffer = Buffer.modify(state.buffer, func(b : Buf): Buf {
                  printer.leaveAnnotation(ann, annotations, b)
                });

                flexGroup = state.flexGroup;
                indentSpaces = state.indentSpaces;
                position = state.position;
                })
          };
        }
      };
    }
  };

  go(initialState)
};
}
