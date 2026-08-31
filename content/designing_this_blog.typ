#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge
#set document(
  title: [Trying to write a blog again]
)
#show: doc => html.html([
  #html.head(
    // Option A: Link to an external CSS file
    html.link(rel: "stylesheet", href: "/style.css")
  )
  #html.body(html.main(doc))
])

#title()

= Roadblocks in writing a blog
Four years ago, I wanted to build a nice website and write my own blog about my
ideas. However, I think 

== making it too complicated
Was writing a static site generator in haskell. Never wrote a blog because I
kept going outside or watching youtube shorts.
== Picking out colors
Yeah, this is kinda dumb but I want good colors damnit. Whenever I try to pick
out some colors I end up picking up a book on color theory and now I can't even
work on my website.

== Never writing anything
Why write when you can watch YouTube Shorts!?

= New principles

== Keep it simple, stupid: Just focus on solving the problem
Should've followed this a while back, but this site is going to be as simply
made as possible. Someone who has minimal background in tech can just use HTML,
CSS, and some commandline tool to make it and host it for free on github pages.
Heck, Claude can do it for them.
== Typst!
I love typst and diagrams and math formulas. 
$ a^2 + b^2 = c^2 $
$ sum_(k=1)^n k = (n(n+1)) / 2 $

#html.elem("div", attrs: (style: "display: flex; justify-content: center; align-items: center;", class: "icon"))[#html.frame(
  diagram(cell-size: 15mm, $
          G edge(f, ->) edge("d", pi, ->>) & im(f) \
          G slash ker(f) edge("ur", tilde(f), "hook-->")
  $)
)]

== Short posts
I'm going to write lots, but keep my posts as short and bitable as they can be.
I have a lot of ideas, but I have to start small and not be a perfectionist.
