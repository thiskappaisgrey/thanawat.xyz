#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

#set document(
  title: [An Agentic Idea]
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

= An idea: Rough draft
An idea came to me recently: how do we get Agents(or what I affectionately call
"Clankers") to write more reliable
software and make more reliable decisions in a more explainable way? Before I
dive into the idea, I want to give some context to build up the idea so that
for those of you who are reading who have no background can understand what I'm
doing. How I arrive at the idea is sort of also fundamental to the idea as
well.

The throughline of this draft is roughly, we want agents to do tasks or come to
some outcome. In order to do that, we need to:
- What _resources_ are available to accomplish said task?
- How do we _define_ the tasks and _decompose_ the tasks in an understandable
  way? (Pictorally)
- Within the space of _possible_ solutions, how do we pick the _best one for
  the task_?
- How do we assign the tasks to _agents_ that are specialized to solve said task! 
- _agent scheduling_ - how do we optimally _schedule_ each agent so that we use
  our computing resources optimally?

Each step in the chain, I have an intersting core idea that will be explored in
future blog posts. For now I just wanted to write down a simple core idea to
share with colleagues and friends.

= Worked example: Baking Pie (or making the best pie)
*This is the perfect example* for demonstrating this idea!

Imagine you're at a strawberry pie shop making the best strawberry pie. How
might you do it if you
were in charge of this? You might first start with a recipe for making pie,
which would be represented by this diagram:

/* TODO: Need to figure out how to put the conversation side by side, help me agent pls */
#figure(
  html-diagram(diagram(
    node-stroke: 1pt,
    edge-stroke: 1pt,
    spacing: (5em, 0.5em),
    node((0, -0.25), [], stroke: none, name: <strawberries>),
    node((0, 1.25), [], stroke: none, name: <sugar>),
    node((0, 1.95), [], stroke: none, name: <flour>),
    node((0, 3), [], stroke: none, name: <butter>),
    node((1.3, 0.6), [Make\ Filling], shape: rect, height: 4em, name: <filling-box>),
    node((1.3, 2.45), [Make\ Crust], shape: rect, height: 3em, name: <crust-box>),
    node((2.5, 1.5), [Assemble], shape: rect, height: 6em, name: <assemble>),
    node((4.3, 1.5), [Bake], shape: rect, name: <bake>),
    node((5.3, 1.5), [Pie], shape: fletcher.shapes.pill, fill: luma(230)),
    edge(<strawberries>, <filling-box.north-west>, "-", [Strawberries], label-side: left),
    edge(<sugar>, <filling-box.south-west>, "-", [Sugar], label-side: left),
    edge(<flour>, <crust-box.north-west>, "-", [Flour], label-side: left),
    edge(<butter>, <crust-box.south-west>, "-", [Butter], label-side: left),
    edge(<filling-box>, <assemble.north-west>, "-", [Filling], label-side: left),
    edge(<crust-box>, <assemble.south-west>, "-", [Crust], label-side: left),
    edge(<assemble>, <bake>, "-", [Unbaked pie], label-side: left),
    edge((4.3, 1.5), (5.3, 1.5), "-"),
  )),
  caption: [A recipe for strawberry pie, drawn as a string diagram: ingredients are wires that flow through processes (boxes) into the finished pie],
)


Your pie needs to exist within certain _constraints_. For example, because you picked
strawberry pie, you know that you aren't going to be sourcing oranges when
looking for ingredients(I'm trying to invoke a "type system" here for those
that get it). However, within the space of valid ingredients, we might have
some _cost function_ to try to pick the "best" strawberry + flour + some other
ingredient to make the best pie! In jargon, you might use a type system to
_eliminate_ invalid combinations of ingredients, and some sort of cost function
or SMT solving to figure out _of the valid combinations_ what is the "best"
combination for your shop.



To come up with the best filling strategy - we might have two "agents" working
together, one taster agent that checks, given some hidden criteria, that the
recipe is "good" and one maker agent that comes up with recipes based on the
information they recieve from the taster. These two agents would have to
communicate to each other on what the best pie
recipe is. They might have these two threads(back and forth conversation):

#table(
  columns: (1fr, 1fr),
  stroke: 0.5pt + gray,
  align: left,
  [*Maker*], [*Taster*],
  [I'm going to make strawberry filling. I'm going
  to try to make it really sweet because I like
  sweet and Americans like sweet. I'll put in 90g
  of sugar and 10g of strawberries.], [],
  [→ #text(style: "italic")[sends recipe to taster]], [],
  [], [I think Americans tend to make things really
  sweet, and you can't taste the nuance in the
  strawberries. #text(style: "italic")[This strawberry filling is too
  sweet!]],
  [], [← #text(style: "italic")[sends verdict to maker]],
  [This taster sucks. He doesn't like my sweet
  filling. Is he some sort of European? "What do
  you mean by that?"], [],
  [→], [],
  [], [Well, I generally think that although
  Americans like sweet things, strawberry fillings
  should be less sweet to bring out the nuance of
  the strawberry.],
  [], [←],
  [Ok. Let me reduce the sugar to 40g and increase
  strawberries to 60g. #text(style: "italic")[sends new filling to taster]], [],
  [→], [],
  [], [Much better!],
)

Result: Good Filling.

This sort of _message passing_ is _codependent_ and can't be represented by a
string diagram, but does encapsulate a _specific process_ in which a process
for making something is refined.




Obviously, there are only so many agents in your fictional pie shop! This
requires a _scheduler_ to pick which order in which the agents perform tasks,
and agents often times need to _context switch_ between tasks as well!
Obviously in the agentic world we can _design harnesses_ to make the agent
specialize in a specific field and then run the agent _in machines_
distributedly!

This is a dummy example, but we can imagine that this process is _going on
right now in any decently large organization_. Thus, why can't we apply this
idea into our Agentic AIs as well to get the best result out of these clankers
and be a mini CEO of our vibe coded apps?


I will go into depth with some of these ideas in smaller blog posts in a bit
(not to any formality), but just enough to outline the idea. Many of these
ideas were inspired by reading about string diagrams and "the society of mind"
and I will reference them here in the references.

= Some of my References
Just so you know, I just read the table of contents of most of these and didn't
really read them through ;)
- _Society of Mind_ Marvin Minsky
- https://www.unison-lang.org/
- https://mastra.ai/books/principles-of-building-ai-agents
- https://arxiv.org/abs/1803.05316
- github.com/HigherOrderCO/bend  (Interaction Nets!)
- https://www.cs.ox.ac.uk/ucs/hoarebook.pdf
- https://i.warosu.org/data/sci/img/0163/64/1725651701869705.pdf ( Types and Programming Languages ) 
- https://aurae.io/
- https://it4innovations.github.io/hyperqueue/stable/

= Working on this
I will be working on this sporadically as this is my side quest and not my main
goal. My main goal is.. to get a girlfriend. It's a rough and long ever lasting
journey without end.
