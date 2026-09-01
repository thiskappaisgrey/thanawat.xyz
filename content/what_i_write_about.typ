
#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

#set document(
  title: [What I write about]
)
#show: doc => html.html([
  #html.head(
    // Option A: Link to an external CSS file
    html.link(rel: "stylesheet", href: "/style.css")
  )
  #html.body(html.main(doc))
])

#let html-diagram(body) = context {
  if target() == "html" {
    html.elem("div", attrs: (style: "display: flex; justify-content: center; align-items: center;", class: "icon"))[#html.frame(body)]
  } else {
    body
  }
}

#title()

I only write about the things _I experience_ and what I find to be _true_ in
_my experience_. Here I'm not talking about reproducable, scientific, truth,
although, if you also accept these things in you I would hope they are
reproducable for you as well, but experiental truth - the kind of truth you
experience for yourself and recognize that it makes you happy. The kind of
truth that nourishes your spirit and keeps you moving forward. Or, really, it
could all be my psychological defense mechanisms and not true at all! Whether
my ideas speak to you is really up to you to decide!
