# Frontend / Backend Integration Plan

## Goal

Turn the current terminal-driven Haskell card game into:

1. a continuously running backend process
2. accepting WebSocket connections
3. exchanging all input/output over the socket
4. exposing full game state snapshots for frontend sync
5. paired with a rudimentary Angular frontend to play the game

## Agreed Scope

- One game session per WebSocket connection
- One browser tab controls both players in hotseat mode
- Backend sends only full state snapshots plus explicit prompts for the first version
- No dedicated animation event stream yet
- Adapt the current effect-driven loop first; no up-front rewrite to a full explicit game state machine
- Angular app lives under `frontend/`
- Frontend and backend remain separate dev servers
- Local-only development is sufficient for the first version
- Use plain Warp + WebSockets instead of Scotty

## Current State

- The backend currently runs as a terminal loop in `app/Main.hs`.
- `GameEffects` abstracts command input, numeric choice input, and log output.
- `Game.hs` owns the outer game loop and terminal-oriented prompts.
- `Interpreter.Game` mutates authoritative game state and emits logs, but there is no event model yet.
- There is no HTTP server, no WebSocket layer, no JSON serialization, and no frontend workspace.

## Proposed Direction

### Backend

- Replace the terminal executable entry point with a Warp-based long-running web app.
- Accept WebSocket connections from the frontend.
- Keep the backend authoritative for all game state changes.
- Introduce a structured protocol:
  - frontend -> backend: commands / selections / session actions
  - backend -> frontend: state snapshots + prompt messages
- Reuse the existing `GameEffects` abstraction by swapping terminal interpreters for connection-backed interpreters.

### Transport

- Use WebSockets for interactive gameplay.
- Likely message categories:
  - `hello` / `connected`
  - `game_state`
  - `prompt`
  - `command`
  - `choice_response`

### Frontend

- Add a separate Angular app under a new directory, likely `frontend/`.
- Connect to the WebSocket backend.
- Render:
  - both players
  - hand
  - field
  - graveyard
  - current prompts
  - log / event stream
- Start with a minimal playable UI rather than a polished card game presentation.

## Recommended Initial Scope

This is now the agreed first cut and should be treated as the implementation target.

## Concerns / Technical Risks

### 1. Terminal loop vs socket-driven prompt flow

The current architecture is close to working for WebSockets because input/output is already abstracted via effects.
The main issue is that prompts are implicit in logs plus blocking reads.

For a frontend, we will likely want explicit prompt messages such as:

- choose command
- choose one target
- choose one option
- choose multiple targets

That may require making prompts more structured than they currently are.

### 2. Serialization surface is large

If we choose “send the full state”, we will need JSON instances for a substantial portion of `DataTypes` / `GameState`.
That is straightforward, but it will touch many types.

### 3. The current game is not fully implemented

Attacks are still not implemented in `Game.hs`.
That is fine for this migration, but the frontend will need to reflect that current limitation cleanly.

## Suggested Implementation Order

1. Define the transport protocol types.
2. Add JSON serialization for game state and protocol messages.
3. Replace terminal `Main` with Warp + WebSocket server.
4. Implement connection-backed interpreters for input/output effects.
5. Emit state snapshots and prompt messages.
6. Scaffold Angular frontend.
7. Implement WebSocket client service.
8. Build a minimal playable UI.
9. Iterate on animation/event richness later.

## Alternatives

### Alternative 1: backend only emits full state, no event stream initially

- Much simpler
- Frontend can still function
- But animation quality will be weak because transitions are inferred rather than declared

### Alternative 2: explicit backend game state machine first

- Cleaner long-term architecture
- Better protocol design
- But larger up-front refactor before any frontend is visible

## Next Step

This file now reflects the agreed plan and can be used directly as the execution plan for implementation.
