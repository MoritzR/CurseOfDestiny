# CurseOfDestiny

[![Build Status](https://travis-ci.org/MoritzR/CurseOfDestiny.svg?branch=master)](https://travis-ci.org/MoritzR/CurseOfDestiny)

A pet project to play around with haskell.

## Run locally

The project now consists of:

- a Haskell backend with a WebSocket server
- an Angular frontend in `frontend/`

Both are expected to run inside the `devenv` shell from `devenv.nix`.

### 1. Enter the development shell

```sh
devenv shell
```

### 2. Start the backend

From the repository root, in one terminal:

```sh
cabal run cod-exe
```

This starts the backend on `http://127.0.0.1:8080` with the WebSocket endpoint on the same port.

### 3. Start the frontend

In a second terminal, also inside `devenv shell`:

```sh
cd frontend
npm install
npm start
```

The Angular dev server starts on `http://127.0.0.1:4200` and connects to the backend at `ws://127.0.0.1:8080`.

### 4. Open the app

Open `http://127.0.0.1:4200` in the browser.

## Tests
To update the golden/snapshot tests, run `hgold`.
