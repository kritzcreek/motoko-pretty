import List "mo:base/List";

module {

type Queue<B> = List.List<List.List<B -> B>>;
public type T<B> = { buffer : B; queue : Queue<B>; };

public func branch<B>(buf: T<B>) : T<B> {
  {
    buffer = buf.buffer;
    queue = List.push<List.List<B -> B>>(List.nil(), buf.queue);
  }
};

public func commit<B>(buf: T<B>) : T<B> {
  {
    queue = List.nil();
    buffer = List.foldRight<List.List<B -> B>, B>(
      buf.queue,
      buf.buffer,
      func(fs, b) {
        List.foldRight<B -> B, B>(fs, b, (func(f, x) { f(x) }))
      }
    )
  }
};

public func revert<B>(buf : T<B>) : T<B> {
  {
    buffer = buf.buffer;
    queue = List.drop(buf.queue, 1);
  }
};

public func modify<B>(buf : T<B>, f: B -> B): T<B> {
  switch (buf.queue) {
    case null {
      {
        buffer = f(buf.buffer);
        queue = buf.queue;
      }
    };
    case (?(fs, queue)) {
      {
        buffer = buf.buffer;
        queue = List.push(List.push(f, fs), queue);
      }
    };
  }
};

public func get<B>(buf : T<B>) : B {
  commit(buf).buffer
};

public func new<B>(b : B): T<B> {
  { queue = null; buffer = b }
};

public func isBranching<B>(buf: T<B>): Bool {
  not List.isNil(buf.queue)
};
}
