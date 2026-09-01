#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge
#set document(
  title: [Title]
)
#show: doc => html.html([
  #html.head(
    // Option A: Link to an external CSS file
    html.link(rel: "stylesheet", href: "/style.css")
  )
  #html.body(html.main(doc))
])

#title()

= 
