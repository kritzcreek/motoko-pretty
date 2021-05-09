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
