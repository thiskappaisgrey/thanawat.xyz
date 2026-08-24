#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

= this is my blog

#html.frame(
  diagram(cell-size: 15mm, $
          G edge(f, ->) edge("d", pi, ->>) & im(f) \
          G slash ker(f) edge("ur", tilde(f), "hook-->")
  $)
)
