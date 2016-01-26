# graphviz-dot-mode
graphviz-dot-mode is a editing mode for writing scripts to generate graphs, this
scripting language is called dot(See [drawing graphs with dot](http://www.graphviz.org/Documentation/dotguide.pdf), [The dot language](http://www.graphviz.org/doc/info/lang.html)).

To use this tool, you need to install dot on your system.

## Example
```
digraph structs {
	node [shape=record];
	struct1 [label="<f0> left | <f1> middle | <f2> right"];
	struct2 [label="<f0> one | <f1> two"];
	struct3 [label="hello \nworld | {b | {c |<here> d| e} | f}| g | h"];
	struct1:f1 -> struct2:f0;
	struct1:f2 -> struct3:here;
}
```

C-c c --> compile
C-c p --> preview
