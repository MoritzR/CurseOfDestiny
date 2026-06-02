# Frontend

This is a manually scaffolded Angular standalone app for the first local hotseat version.

## Expected backend

- WebSocket endpoint: `ws://127.0.0.1:8080`
- Protocol: JSON messages matching `src/app/protocol.ts`

## Local development

Run the frontend inside the repository `devenv` shell.

1. From the repository root: `devenv shell`
2. `cd frontend`
3. `npm install`
4. `npm start`

The dev server runs on `http://127.0.0.1:4200`.
