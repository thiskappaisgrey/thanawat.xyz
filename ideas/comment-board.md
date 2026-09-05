# Idea: turn the blog into a comment board (highlight-to-comment, like code review)

Status: not started, static site stays as-is for now. Revisit later.

## Motivation
Static site (Zola) works fine but comments/discussion are currently nonexistent.
Want something like GitHub PR review: select a chunk of text on a post, leave a
comment anchored to it, see others' comments live.

## Stack
- **Gleam** on the BEAM (my main language) instead of Go/Python.
  - Likely `wisp` or `mist` for the HTTP layer, `gleam_sqlight` (or similar) for SQLite.
  - Need to check maturity of a DataStar SDK/helpers in Gleam, or just hand-roll
    SSE responses (Gleam's actor model should make broadcast-to-many-connections
    reasonably natural — one process per SSE connection, subscribed to a topic).
- **DataStar** on the frontend for reactive fragments over SSE, no SPA build step.
- **SQLite** for storage (comments, moderation state). Litestream or similar for
  backup once this is live, since SQLite needs a persistent disk (no more static
  hosting — would need Fly.io/a small VPS/etc).

## Data model (sketch)
- `posts`: id, slug, rendered content (or render on the fly from source at request time)
- `comments`: id, post_id, anchor, body, author, created_at, status
  - `anchor`: text-quote anchor (prefix + selected text + suffix), à la Hypothesis —
    survives minor edits to the post better than raw char offsets.
  - `status`: pending | visible | hidden

## Highlight-to-comment mechanism
- Client JS captures `window.getSelection()` range on the rendered post.
- Resolve selection to a text-quote anchor (selected text + surrounding context)
  rather than DOM offsets, so it's robust to re-renders.
- Submit anchor + comment body to backend; backend re-renders comments for that
  post as a DataStar SSE fragment, pushed to all connected viewers on that page.

## Moderation (cheap, two-stage)
1. Free heuristics first, reject/hold instantly: per-IP rate limit, link/repetition
   checks, length bounds, basic blocklist.
2. Only comments that pass stage 1 go to a single cheap LLM call (e.g. Haiku) —
   "spam or hostile: yes/no" — before flipping status to visible.
   Keeps LLM usage limited to the minority that survives stage 1.

## Open questions / things to figure out when picking this back up
- How mature is the Gleam HTTP/SSE/SQLite ecosystem for this? (check wisp, mist,
  gleam_sqlight, any actor-based pub/sub examples)
- Auth for commenters — anonymous + name field, or something lighter than full accounts?
- Where does this get hosted, now that it needs a writable disk + long-running process?
- Anchor resolution when a post is edited after comments exist (best-effort fuzzy match,
  fall back to "comment on paragraph N" if the quote can't be found).

## Tradeoff vs. current static setup
Moves off free static hosting onto something with persistent state and a
moderation/abuse surface that doesn't exist today. Worth it only if I actually
want a place for people to leave feedback on posts, not just a novelty feature.
