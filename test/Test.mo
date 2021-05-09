import Debug "mo:base/Debug";
import M "mo:matchers/Matchers";
import Pretty "../src/Pretty";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";

// let suite = S.suite("pretty", []);

// S.run(suite);

Debug.print("Starting:");

let printOpts : Pretty.PrintOptions = {
  pageWidth = 12;
  ribbonRatio = 1.0;
  indentUnit = "    ";
  indentWidth = 4;
};

let hanging = Pretty.alignCurrentColumn(Pretty.textParagraph("a b c d e f g"));
let test1 = Pretty.lines([
    Pretty.words([ Pretty.text("111"), hanging]),
    Pretty.words([ Pretty.text("111111"), hanging]),
    Pretty.words([ Pretty.text("111111111"), hanging])
]);

Debug.print(Pretty.print(Pretty.plainText, printOpts, test1));

let test2 = Pretty.appendSpaceBreak(Pretty.text "11111111", Pretty.text "11111111");
Debug.print(Pretty.print(Pretty.plainText, printOpts, test2));

let test3 = Pretty.appendBreak(Pretty.text "11111111", Pretty.indent(Pretty.text "22222222"));
Debug.print(Pretty.print(Pretty.plainText, printOpts, test3));
