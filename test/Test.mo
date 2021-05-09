import M "mo:matchers/Matchers";
import Pretty "../src/Pretty";
import S "mo:matchers/Suite";
import T "mo:matchers/Testable";

let suite = S.suite("pretty", []);

S.run(suite);
