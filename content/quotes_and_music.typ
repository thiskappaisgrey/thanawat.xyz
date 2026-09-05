#set document(
  title: [Quotes and music]
)
#show: doc => html.html([
  #html.head(
    // Option A: Link to an external CSS file
    html.link(rel: "stylesheet", href: "/style.css")
  )
  #html.body(html.main(doc))
])

#title()

These are quotes and music that I like in no particular order attributed to
other people. I will write in depth about these quotes later in my own blog
posts, but maybe it's good for you yourself to think about what it means!


#show link: it => html.elem("a", attrs: (href: it.dest, target: "_blank", rel: "noopener noreferrer"), it.body)

#show quote.where(block: false): it => {
  ["] + h(0pt, weak: true) + it.body + h(0pt, weak: true) + ["]
  if it.attribution != none [ #linebreak() -- #it.attribution ]
 }


#quote(attribution: [My Gym Coach's dad])[
  Sometimes you need to go all around the world to cross the street.
]

#quote(attribution: [A mentor of mine])[
  Feel your feelings, especially your anger.
]

#quote(attribution: [Another mentor of mine])[
  People are always just #link("https://en.wikipedia.org/wiki/Don_Quixote#Tilting_at_windmills")[tilting at windmills]!
]

#quote(attribution: [Albert Camus])[
 My Revolt! My Passion! My Freedom!
]

#quote(attribution: [Jimmy Cliff])[
  I'd rather be a freeman in my grave, than living as a puppet or a slave.
  ... The harder they come, the harder they fall, one and all.
]

#quote(attribution: [Chris Cornell])[
  To be yourself is all that you can do.
]

#quote(attribution: [My interpretation of some Taoist Sage])[
  The world is a mirror. It mirrors back your inner soul.
]

// #footnote[My friend would always correct me in that it's "happy" not smiling, but I prefer smiling because one cannot be happy all of the time.] 

#quote(attribution: [Camus])[
  One must imagine Sisyphus smiling. (I changed "happy" to "smiling")
]

#quote(attribution: [Me (and Claude)])[
  Talk about your problems to people that will _really_ listen.
]


#quote(attribution: [Me])[
  When you listen to people's issues, listen like a therapist would. Ask them
  questions. Get them to clarify the problem. And then once you have enough
  information, stop being a therapist and actually rouse them into action.
  Talking about feelings is not enough, but _understanding your feelings_ and
  _acting anyways_ is the important part.
]

#quote(attribution: [Me])[
  I get angry at the people that just don't let me be sad. Just let me be sad.
  It's OK to be sad. Being sad is what allows you to be happy.
]

#quote(attribution: [Sir Arthur Conan Doyle])[
  I consider that a man's brain originally is like a little empty attic, and
  you have to stock it with such furniture as you choose. A fool takes in all
  the lumber of every sort that he comes across, so that the knowledge which
  might be useful to him gets crowded out, or at best is jumbled up with a lot
  of other things, so that he has a difficulty in laying his hands upon it. Now
  the skillful workman is very careful indeed as to what he takes into his
  brain-attic. He will have nothing but the tools which may help him in doing
  his work, but of these he has a large assortment, and all in the most perfect
  order. It is a mistake to think that that little room has elastic walls and
  can distend to any extent. Depend upon it there comes a time when for every
  addition of knowledge you forget something that you knew before. It is of the
  highest importance, therefore, not to have useless facts elbowing out the
  useful ones.
]

#quote(attribution: [Sir Arthur Conan Doyle])[
  You see, but you do not observe.
]

#quote(attribution: [Mortimer J. Adler])[
  You can only communicate to others what you can communicate to yourself.
]

#quote(attribution: [Me])[
  Language is a leaky abstraction! Words don't mean what they mean oftentimes,
  and it's up to you to figure out what words really mean.
]

#quote(attribution: [(derived from) Mortimer J. Adler])[
  Learn to come to terms with people. Meaning, learn what their words are,
  understand what they _mean_ by those words. Once you can understand what they
  are communicating behind their words, you really understand them.
]

#quote(attribution: [Karl Marx])[
  In like manner, the beginner who has learned a new language always translates
  it back into his mother tongue, but he assimilates the spirit of the new
  language and expresses himself freely in it only when he moves in it without
  recalling the old and when he forgets his native tongue.
]

= Music. Most are sad music.
- #link("https://www.youtube.com/watch?v=Xkzp6cTBhJc")[Be Yourself]
- #link("https://www.youtube.com/watch?v=ioJizJ93DdQ")[Rex's Blues]
- #link("https://www.youtube.com/watch?v=g4-3TPjRoSQ")[Miss Carousel]
- #link("https://www.youtube.com/watch?v=dF_3w_gXing")[Nothin]
- #link("https://www.youtube.com/watch?v=eMuzFQTpjDE")[Trouble In Mind]
- #link("https://www.youtube.com/watch?v=9EKi2E9dVY8")[Nutshell]
- #link("https://www.youtube.com/watch?v=KSQ0L5EdYMo")[Creep (Stone Temple Pilots)]
- #link("https://www.youtube.com/watch?v=TsvL2poCHug")[Fake Plastic Trees]
- #link("https://www.youtube.com/watch?v=IHvzsDAZCzc")[ลุงขี้เมา]
- #link("https://www.youtube.com/watch?v=UnxZSGRg3Jk")[เดือนเพ็ญ]
- #link("https://www.youtube.com/watch?v=KyA3IaUlUdM")[ทะเลใจ]
- #link("https://www.youtube.com/watch?v=PkBYZdZ3RoI")[Fade Away]
- #link("https://www.youtube.com/watch?v=WnO9WqYUgN8")[Cigarretes in hell]
